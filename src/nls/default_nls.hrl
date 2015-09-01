%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2015 9:20 PM
%%%-------------------------------------------------------------------
-author("Shuieryin").

-define(ENCODING, utf8).
-define(SUPPORT_LANGUAGES, [zh, en]).
-define(IS_VALID_LANG(Lang), begin
                                 length([X || X <- ?SUPPORT_LANGUAGES, X == Lang]) == 1
                             end).

-define(COMMON_NLS_CONTENT, #{
    invalid_command => #{
        zh => <<"无效指令: "/?ENCODING>>,
        en => <<"Invalid command: ">>
    }
}).