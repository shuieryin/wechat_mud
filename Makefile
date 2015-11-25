all: install

install:
	@./config/install.sh

cu: clean_upgrade

clean_upgrade: remove_appup upgrade

remove_appup:
	@rm -f ebin/*.appup

upgrade:
	@#bash -x
	@./config/hcu.sh

run:
	@./config/run.sh

reset:
	git fetch --all
	git reset --hard origin/master

ct:
	#@cd _build/default/rel/wechat_mud
	ct_run -suite wechat_mud_SUITE -logdir ./logs -pa ./_build/default/lib/proper/src
	#@cd ../../../..