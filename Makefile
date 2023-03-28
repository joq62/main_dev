all:
	rm -rf apps/main/src/*~ *~;
	rm -rf apps/main/src/*.beam;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rm -rf  _build;
	rebar3 release;
	rm -rf _build*;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!make
build:
	rm -rf apps/main/src/*~ *~;
	rm -rf apps/main/src/*.beam;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rm -rf  _build;
	rebar3 release;
	rm -rf _build*

clean:
	rm -rf  *~ */*~ src/*.beam tests/*.beam
	rm -rf erl_cra*;
	rm -rf spec.*;
	rm -rf tests_ebin
	rm -rf ebin;
	rm -rf Mnesia.*;
	rm -rf *.dir;
	rm -rf common;
	rm -rf sd;
	rm -rf nodelog;
	rm -rf etcd;
prod:
	rebar3 as prod release;
	rebar3 as prod tar;
	mv _build/prod/rel/main/*.tar.gz ../release 

dev:
	rm -rf apps/main/src/*~ *~;
	rm -rf apps/main/src/*.beam;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rm -rf  _build;
	rebar3 release;
	rebar3 ct
#	_build/default/rel/main/bin/main console
