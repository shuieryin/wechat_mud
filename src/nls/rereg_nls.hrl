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
    info => #{
        zh => <<"指令:rereg\n描述:重新注册人物\n例子:rereg"/?ENCODING>>,
        en => <<"COMMAND:rereg\nDESCRIPTION:Register character\nExample:rereg">>
    }
})).