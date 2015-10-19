all:
	$(eval BASE_VER := 0.1.0)
	#rebar3 relflow init-versions
	rm -rf _build/default/rel/wechat_mud/
	rm -f ebin/wechat_mud.appup
	git tag -d $$(git tag)
	sed -i.bak 's/vsn,\s.*\".*\"/vsn, \"$(BASE_VER)\"/1' src/wechat_mud.app.src
	sed -i.bak 's/\".*\" %% relflow/\"$(BASE_VER)\" %% relflow/1' rebar.config
	git tag -a v$(BASE_VER) -m "base release"
	rebar3 compile
	rebar3 release

upgrade:
	rebar3 compile
	$(eval CURRENT_TAG := git describe --abbrev=0 --tags)
	$(eval CURRENT_VERSION := $(CURRENT_TAG) | sed -e "s/v//g")
	git commit -a -m upgrade_from_$$($(CURRENT_TAG))

	rebar3 relflow -u $$($(CURRENT_TAG))
	rebar3 release relup -u $$($(CURRENT_VERSION)) tar

	$(eval LATEST_VERSION := git describe --abbrev=0 --tags | sed -e "s/v//g")
	mv _build/default/rel/wechat_mud/wechat_mud-$$($(LATEST_VERSION)).tar.gz _build/default/rel/wechat_mud/releases/$$($(LATEST_VERSION))/wechat_mud.tar.gz
	./_build/default/rel/wechat_mud/bin/wechat_mud install $$($(LATEST_VERSION))
	#sed -i.bak 's/$(OLD_VER)/$(NEW_VER)/1' reltool.config
	#rebar3 relup
	#rebar3 install $(NEW_VER)

t:
	$(eval CURRENT_TAG = `git describe --abbrev=0 --tags`) 
	@echo $(CURRENT_TAG)
