"""Fabric file for building haverer-api on the Vagrant build machine."""

import contextlib
import errno
import os
import tempfile

from fabric.api import cd, env, get, lcd, local, settings, sudo, put, task, run
from fabric.context_managers import path


# The user that we're going to run the builds as. Actual value must match the
# value in the ansible playbook.
CABAL_USER = 'cabal-builder'


# Paths to Haskell binaries necessary for building. Copied from the Ansible
# playbook (where these values come from the particular GHC packages that we
# use).
CABAL_PATH = os.pathsep.join([
    '/opt/ghc/7.8.4/bin',
    '/opt/cabal/1.20/bin',
    '/opt/happy/1.19.4/bin',
    '/opt/alex/3.1.3/bin',
])


BINARY_OUTPUT_PATH = 'dist/vagrant/haverer-api-latest/'


# XXX: This is a terrible hack to allow us to have haverer as a dependency on
# the build server. Fixes include making a real CI server (Hydra? Nix?) or
# putting both trees into one repo and moving the build infrastructure up a
# level.
SOURCE_DEPENDENCIES = [
    '../haverer',
]


def ensure_directory(directory):
    try:
        os.makedirs(directory)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise


# XXX: Rather than use a tarball here, upload the whole directory.
def _source_dist(prefix, revid, directory):
    """Build a tarball of the given revid.

    :param prefix: The prefix of the tarball. Created file will be
        $prefix.tar.gz, and will untar into a directory called $prefix/
    :param revid: The git revision ID to export.
    :param path: The local directory to store the tarball in.
    :return: The full path to the created tarball.
    """
    output_path = '{directory}/{prefix}.tar.gz'.format(
        directory=directory, prefix=prefix)
    cmd = 'git archive --prefix={prefix}/ -o {path} {revid}'
    local(cmd.format(prefix=prefix, revid=revid, path=output_path))
    return output_path


def make_git_snapshot(source_dir=None, target_dir=None, revid='HEAD',
                      name=None):
    if source_dir is None:
        source_dir = os.getcwd()
    if target_dir is None:
        target_dir = tempfile.mkdtemp()
    if name is None:
        name = os.path.basename(os.path.abspath(source_dir))
    ensure_directory(target_dir)
    with lcd(source_dir):
        revno = local('git rev-parse {}'.format(revid), capture=True).strip()
        return _source_dist('{}-{}'.format(name, revno), revno, target_dir)


def tarball_directory(filename):
    return local('tar -tf {} | head -1'.format(filename), capture=True).strip()


def upload_tarball(source_file, remote_dir):
    result = put(source_file, '/tmp/')
    if result.failed:
        raise RuntimeError("Could not upload {}".format(source_file))
    [remote_path] = list(result)
    with cd(remote_dir):
        sudo('tar -xf {}'.format(remote_path))
    return os.path.join(remote_dir, tarball_directory(source_file))


@contextlib.contextmanager
def remote_temp_dir():
    temp_dir = run('mktemp -d').strip()
    try:
        yield temp_dir
    finally:
        run('rm -rf {}'.format(temp_dir))


def upload_source_dep(source_dir, target_dir):
    source_dir = os.path.abspath(source_dir)
    tarball_path = make_git_snapshot(source_dir)
    return upload_tarball(tarball_path, target_dir)


def upload_source_deps(src_deps, target_dir):
    paths = []
    for dep in src_deps:
        paths.append(upload_source_dep(dep, target_dir))
    return paths


@task
def vagrant():
    """Configure other tasks to operate on Vagrant instance."""
    # XXX: Copied from
    # http://sysadminpy.com/sysadmin/2011/04/30/use-fabric-on-vagrant-instances/
    #
    # This is really crummy. Instead of hardwiring some things, it should just
    # load all of the config from Vagrant.

    # XXX: fab now has a way to temporarily adjust the environment. use that
    # instead.

    env.user = 'vagrant'
    env.hosts = ['127.0.0.1:2222']

    # use vagrant ssh key
    result = local('vagrant ssh-config | grep IdentityFile', capture=True)
    env.key_filename = result.split()[1]


@task
def source_to_binary(source_file):
    """Upload source code, build it, and return the path to the binary."""
    sandbox_dir = '/home/{}'.format(CABAL_USER)
    [remote_path] = list(put(source_file, '/tmp/'))
    with cd(sandbox_dir):
        with settings(sudo_user=CABAL_USER,
                      sudo_prefix="sudo -S -i -p '{}'".format(
                          env.sudo_prompt)):
            with path(CABAL_PATH, behavior='prepend'):
                deps = upload_source_deps(SOURCE_DEPENDENCIES, sandbox_dir)
                sudo('tar -xf {}'.format(remote_path))
                sudo('cabal sandbox init --sandbox .')
                for src_dir in deps:
                    sudo('cabal sandbox add-source {}'.format(src_dir))
                src_dir = tarball_directory(source_file)
                with cd(src_dir):
                    sudo('cabal sandbox init --sandbox {}'.format(sandbox_dir))
                    sudo('cabal install --dependencies-only')
                    sudo('cabal build')
    return '/home/{}/{}'.format(CABAL_USER, src_dir)


def _binary_dist(tree_path, output_path):
    with cd(tree_path):
        run('cp -r config/ {}/'.format(output_path))
        run('cp -r static/ {}/'.format(output_path))
        run('cp dist/build/haverer-api/haverer-api {}/'.format(output_path))


@task
def bdist(tree_path):
    """Download the build from the server."""
    # XXX: Include the revid in the filename too
    with remote_temp_dir() as temp_dir:
        _binary_dist(tree_path, temp_dir)
        get('{}/*'.format(temp_dir), BINARY_OUTPUT_PATH)
        # XXX: get() doesn't preserve permissions.
        local('chmod ugo+x {}/haverer-api'.format(BINARY_OUTPUT_PATH))


@task
def sdist():
    """Build a tarball of current HEAD, and store it in dist/src/."""
    directory = os.path.join('dist', 'src')
    return make_git_snapshot(target_dir=directory)


@task
def build():
    """Build current HEAD on the server and download the binary."""
    source_tarball = sdist()
    build_tree = source_to_binary(source_tarball)
    bdist(build_tree)
