# Deployment

*Notes on how I intend to deploy this application*

*Eventually, this should become a guide on the deployment process*

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

`vagrant up`
`vagrant provision`

### Build the binary

`fab vagrant build`

This will:

 1. Make a source distribution from HEAD
 2. Upload it to the Vagrant instance
 3. Build it on that Vagrant instance
 4. Download a tarball containing the binary and necessary configs


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
