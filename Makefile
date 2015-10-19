all:
	rebar3 relflow init-versions
	rm -rf _build/default/rel/wechat_mud/
	git checkout .
	rm -f ebin/wechat_mud.appup
	git tag -d $(git tag)
	rebar3 compile
	rebar3 release

upgrade:
	rebar3 compile
	CURRENT_TAG = $(git describe --abbrev=0 --tags)
	CURRENT_VERSION = ${CURRENT_TAG} | sed -e "s/v//g"

	rebar3 relflow -u ${CURRENT_TAG}
	rebar3 release relup -u ${CURRENT_VERSION} tar

	LATEST_VERSION = $(git describe --abbrev=0 --tags) | sed -e "s/v//g"
	mv _build/default/rel/wechat_mud/wechat_mud-${LATEST_VERSION}.tar.gz _build/default/rel/wechat_mud/releases/${LATEST_VERSION}/wechat_mud.tar.gz
	./_build/default/rel/wechat_mud/bin/wechat_mud install ${LATEST_VERSION}
	#sed -i.bak 's/$(OLD_VER)/$(NEW_VER)/1' reltool.config
	#rebar3 relup
	#rebar3 install $(NEW_VER)
