"""Fabric file for building haverer-api on the Vagrant build machine."""

import contextlib
import errno
import os

from fabric.api import cd, env, get, local, settings, sudo, put, task, run
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


@task
def vagrant():
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


def tarball_directory(filename):
    return local('tar -tf {} | head -1'.format(filename), capture=True).strip()


@task
def source_to_binary(source_file):
    """Upload source code, build it, and return the path to the binary."""
    [remote_path] = list(put(source_file, '/tmp/'))
    with cd('/home/{}'.format(CABAL_USER)):
        with settings(sudo_user=CABAL_USER,
                      sudo_prefix="sudo -S -i -p '{}'".format(env.sudo_prompt)):
            sudo('rm -rf *'.format(CABAL_USER))
            sudo('tar -xf {}'.format(remote_path))
            with path(CABAL_PATH, behavior='prepend'):
                src_dir = tarball_directory(source_file)
                with cd(src_dir):
                    sudo('cabal install --dependencies-only')
                    sudo('cabal build')
    return '/home/{}/{}'.format(CABAL_USER, src_dir)


@contextlib.contextmanager
def remote_temp_dir():
    temp_dir = run('mktemp -d').strip()
    try:
        yield temp_dir
    finally:
        run('rm -rf {}'.format(temp_dir))


def _binary_dist(tree_path, output_path):
    with remote_temp_dir() as temp_dir:
        with cd(tree_path):
            run('cp -r config/ {}/'.format(temp_dir))
            run('cp -r static/ {}/'.format(temp_dir))
            run('cp dist/build/haverer-api/haverer-api {}/'.format(temp_dir))
        with cd(temp_dir):
            run('tar -czf {} *'.format(output_path))


@task
def bdist(tree_path):
    with remote_temp_dir() as temp_dir:
        tarball_path = os.path.join(temp_dir, 'haverer-api-x86_64-linux.tar.gz')
        _binary_dist(tree_path, tarball_path)
        get(tarball_path)


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


@task
def sdist():
    """Build a tarball of current HEAD, and store it in dist/src/."""
    directory = os.path.join('dist', 'src')
    try:
        os.makedirs(directory)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise
    return _source_dist('haverer-api-latest', 'HEAD', directory)


@task
def build():
    source_tarball = sdist()
    build_tree = source_to_binary(source_tarball)
    bdist(build_tree)
