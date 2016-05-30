all: install_rebar3 install

install_rebar3:
	@./config/install_rebar3.sh

install:
	@./config/rebar3 install

run:
	@./_build/default/rel/wechat_mud/bin/wechat_mud console

build:
	@./config/rebar3 build

hcu:
	@./config/rebar3 hcu