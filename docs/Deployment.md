# Deployment

*Notes on how I intend to deploy this application*

*Eventually, this should become a guide on the deployment process*

## Prerequisites

* ansible 1.5+
* vagrant 1.7.2
* fabric 1.10.1
* An AWS account

## Requirements

* One button build step
* One button deploy build step
* Deploy to laptop and run clients from it
* Version controlled configuration
* No secrets in version control
* Develop on OS X 10.10
* As cheap to run in production as possible
* One button rollbacks

## Procedure

### Set up the build server

```
vagrant up
vagrant provision
```

### Build the binary

`fab vagrant build`

This will:

 1. Make a source distribution from HEAD
 2. Upload it to the Vagrant instance
 3. Build it on that Vagrant instance
 4. Download a tarball containing the binary and necessary configs

### Provision a service

* Install the
[ec2 inventory script](http://docs.ansible.com/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script)
* Make sure `localhost` is listed in a file under `/etc/ansible/hosts`
* `ansible-playbook provisioning.yml`

**Note**: Assumes the key is called "mumak aws"

### Deploy to production

```
ansible-playbook \
  -e binary_path=$PATH_TO_BINARY \
  -e local_ssl_certificate_path=$PATH_TO_CERT \
  -e local_ssl_key_path=$PATH_TO_KEY \
  -e local_ssl_ca_path=$PATH_TO_INTERMEDIATE_CERTIFICATES \
  production.yml
```

This will set up all the hosts that are tagged with `Name=haverer-api`.

Current build puts binary in `127.0.0.1-2222/haverer-api-x86_64-linux.tar.gz`.

For EC2, need to provide path to private SSH key using `-i`.

## References

### General ideas

* [The Twelve-Factor App](http://12factor.net/)

### Ansible

* [AWS guide](http://docs.ansible.com/guide_aws.html)
* [Vagrant guide](http://docs.ansible.com/guide_vagrant.html)

### Yesod

* [Basics](http://www.yesodweb.com/book/basics)
* [Deploying your web-app](http://www.yesodweb.com/book/deploying-your-webapp)

## Alternatives considered

I tried using [Nix](http://nixos.org/nix/) in the hope of being able to use
[NixOps](http://nixos.org/nixops/), but to get it working on OS X 10.10 took
way too much effort.
