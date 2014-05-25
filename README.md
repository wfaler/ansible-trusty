# Development environment install with Ansible
This set of Ansible scripts sets up a complete development environment on Ubuntu 14.04 for:

- Haskell (with Emacs set up)
- Scala with sbt
- Javascript with Node and npm

Remove references to node, npm, nginx, java and sbt if you want just a clean Haskell environment.

The scripts also remove a bunch of Ubuntu bundled junk.
To run this do the following after changing the hosts file appropriately:

	ansible-playbook dev-machine.yml -i hosts --ask-pass --ask-sudo-pass

Log on to the host and run (this kept failing via ansible for some reason):

	git clone -b haskell --depth=1 git://github.com/dysinger/el-get ~/.emacs.d/el-get-haskell

Start Emacs on the host and let it download the required Haskell packages. Emacs should now work as a Haskell IDE with auto-complete and all!

## Testing the set-up with Haskell
Follow [Tim Dysingers instructions](http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html) under the heading **Test Spin** after running the following to create an empty hello world Cabal project (lifted from the very same blog post):

	# initialize a project
	mkdir -p ~/src/haskell-hello
	cd ~/src/haskell-hello
	cabal init

Then:

	touch LICENSE
	cabal sandbox init
	# create a Main.hs haskell file to compile
	perl -p -i -e 's/^.*main-is.*$/  main-is: Main.hs/' haskell-hello.cabal
	echo 'main = putStrLn "hello world"' > Main.hs
	# compile & run
	cabal install
	./.cabal-sandbox/bin/haskell-hello

## Credits & acknowledgements
Without [Tim Dysingers](https://twitter.com/dysinger) help and excellent [blog post](http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html) from which much of this is derived and just translated into Ansible automation scripts, none of this would have been possible.
