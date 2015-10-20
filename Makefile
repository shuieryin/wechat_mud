all:
	$(eval BASE_VER := 0.1.0)
	rebar3 relflow init-versions
	rm -rf _build/default/rel/wechat_mud/
	rm -f ebin/wechat_mud.appup
	git tag -d $$(git tag)
	sed -i.bak 's/vsn,\".*\"/vsn,\"$(BASE_VER)\"/1' src/wechat_mud.app.src
	sed -i.bak 's/\".*\" %% relflow/\"$(BASE_VER)\" %% relflow/1' rebar.config
	rm -f rebar.config.bak
	rm -f src/wechat_mud.app.src.bak
	#git commit -a -m "$(BASE_VER) release"
	rebar3 compile
	rebar3 release
	git tag -a v$(BASE_VER) -m "$(BASE_VER) release"

upgrade:
	bash -x ./config/hcu.sh

run:
	./config/run.sh
