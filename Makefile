# all - for build and run test after a fresh clone
all: install_rebar3 ck ct

install_rebar3:
	@./config/install_rebar3.sh

install:
	@./config/rebar3 install

run:
	@redis-server & ./_build/default/rel/wechat_mud/bin/wechat_mud console

compile:
	@./config/rebar3 compile

build:
	@./config/rebar3 build

hcu:
	@./config/rebar3 hcu

reset:
	@git fetch --all; git reset --hard origin/master

app_deps:
	@./_build/default/lib/recon/script/app_deps.erl; dot -T png -O app-deps.dot; rm -f app-deps.dot app-deps.dot.png

crash_dump:
	@./_build/default/lib/recon/script/erl_crashdump_analyzer.sh erl_crash.dump

TS=1
queue_fun:
	@awk -v threshold=${TS} -f _build/default/lib/recon/script/queue_fun.awk erl_crash.dump

ct:
	@./config/rebar3 do ct -c, cover -v

ck: build
	@./config/rebar3 ck

br: build run

ckr: ck run

edoc:
	@./config/rebar3 edoc

# upgrade dependency
updep:
	@${PWD}/config/updep.sh ${lib}