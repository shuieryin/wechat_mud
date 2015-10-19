all:
	$(eval BASE_VER := 0.1.0)
	-rebar3 relflow init-versions
	rm -rf _build/default/rel/wechat_mud/
	rm -f ebin/wechat_mud.appup
	git tag -d $$(git tag)
	sed -i.bak 's/vsn,\".*\"/vsn,\"$(BASE_VER)\"/1' src/wechat_mud.app.src
	sed -i.bak 's/\".*\" %% relflow/\"$(BASE_VER)\" %% relflow/1' rebar.config
	git commit -a -m "$(BASE_VER) release"
	rebar3 compile
	rebar3 release
	git tag -a v$(BASE_VER) -m "$(BASE_VER) release"

upgrade:
	bash -x ./config/hcu.sh

t:
	@echo CURRENT_TAG=$$(git describe --abbrev=0 --tags)
	@echo $$CURRENT_TAG sdjklf
