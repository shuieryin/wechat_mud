all:
	$(eval BASE_VER := 0.1.0)
	rm -rf _build/default/rel/wechat_mud/
	git tag -d $$(git tag)
	sed -i.bak 's/vsn,\".*\"/vsn,\"$(BASE_VER)\"/1' src/wechat_mud.app.src
	sed -i.bak 's/\".*\" %% relflow/\"$(BASE_VER)\" %% relflow/1' rebar.config
	rm -f rebar.config.bak
	rm -f src/wechat_mud.app.src.bak
	rebar3 release

upgrade:
	#bash -x
	./config/hcu.sh

run:
	./config/run.sh
