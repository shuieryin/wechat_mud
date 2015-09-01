%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2015 9:57 PM
%%%-------------------------------------------------------------------
-author("Shuieryin").

-include("default_nls.hrl").

-define(NLS(NlsKey, Lang),
    begin
        maps:get(Lang, maps:get(NlsKey, ?NLS_CONTENT))
    end
).

-define(NLS_CONTENT, maps:merge(?COMMON_NLS_CONTENT, #{
    welcome_back => #{
        zh => <<"欢迎回来!"/?ENCODING>>,
        en => <<"Welcome back!">>
    },

    message_type_not_support => #{
        zh => <<"暂不支持该类型信息"/?ENCODING>>,
        en => <<"This message type is currently not supported">>
    }

})).