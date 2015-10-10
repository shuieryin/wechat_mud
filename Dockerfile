FROM shuieryin/erlang:latest

MAINTAINER Collin Guo <shuieryin@gmail.com>
LABEL "version"="0.1" \
	"description"="Wechat mud game." \
	"author"="Collin Guo"

ENV WORKSPACE_PATH=/root/workspaces

WORKDIR $WORKSPACE_PATH
#Install & Run wechat_mud
RUN  git clone https://github.com/shuieryin/wechat_mud.git \
	&& cd wechat_mud \
	&& make \
	&& chmod 755 rel/run.sh
	#&& cd ..

#Dockerfile engine by passes environment variables in CMD, so the full path is hard coded as below. Docker version 1.8.2.
CMD ["/root/workspaces/wechat_mud/rel/run.sh"]

