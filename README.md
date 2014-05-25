# Development environment install with Ansible
This set of Ansible scripts sets up a complete development environment on Ubuntu 14.04 for:

- Haskell (with Emacs set up)
- Scala with sbt
- Javascript with Node and npm

The scripts also remove a bunch of Ubuntu bundled junk.
To run this do the following after changing the hosts file appropriately:

	ansible-playbook dev-machine.yml -i hosts --ask-pass --ask-sudo-pass

Log on to the host and run (this kept failing via ansible for some reason):

	git clone -b haskell --depth=1 git://github.com/dysinger/el-get ~/.emacs.d/el-get-haskell

Start Emacs on the host and let it download the required Haskell packages. Emacs should now work as a Haskell IDE with auto-complete and all!
