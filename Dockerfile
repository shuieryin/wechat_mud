FROM shuieryin/erlang:latest

MAINTAINER Collin Guo <shuieryin@gmail.com>

ENV WORKSPACE_PATH=/root/workspaces

LABEL "version"="0.1.0" \
	"description"="Wechat mud game." \
	"author"="Collin Guo"

WORKDIR $WORKSPACE_PATH
#Install & Run wechat_mud
RUN  git clone https://github.com/shuieryin/wechat_mud.git \
	&& cd wechat_mud \
	&& git config --global user.email "shuieryin@gmail.com" \
	&& git config --global user.name "shuieryin" \
	&& make \
	&& chmod 755 rel/run.sh
	#&& cd ..

#Dockerfile engine by passes environment variables in CMD, so the full path is hard coded as below. Docker version 1.8.3.
CMD ["/root/workspaces/wechat_mud/rel/run.sh"]

