all: install_rebar3 ck build ct

install_rebar3:
	@./config/install_rebar3.sh

ck:
	@./config/rebar3 ck

build:
	@./config/rebar3 build

ct:
	@./config/rebar3 cmt

cu: clean_upgrade

clean_upgrade: remove_appup upgrade

remove_appup:
	@rm -f ebin/*.appup

upgrade:
	@#bash -x
	@./config/hcu.sh

run:
	@./config/run.sh