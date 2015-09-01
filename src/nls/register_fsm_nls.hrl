%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2015 9:18 PM
%%%-------------------------------------------------------------------
-author("Shuieryin").

-include("nls_default.hrl").

-define(NLS(NlsKey, Lang),
    begin
        maps:get(Lang, maps:get(NlsKey, ?NLS_CONTENT))
    end
).

-define(NLS_CONTENT, maps:merge(?COMMON_NLS_CONTENT, #{
    select_lang => [<<"请选择语言:\nPlease select language:\n"/utf8>>, [[atom_to_binary(X, utf8), <<"\n">>] || X <- ?SUPPORT_LANGUAGES]],

    gender_label => #{
        zh => <<"性别: "/?ENCODING>>,
        en => <<"Gender: ">>
    },

    born_month_label => #{
        zh => <<"出生月份: "/?ENCODING>>,
        en => <<"Born month: ">>
    },

    please_input_sex => #{
        zh => <<"请输入角色的性别:\n(男-m / 女-f)"/?ENCODING>>,
        en => <<"Please input sex:\n(Male-m / Female-f)">>
    },

    invalid_sex => #{
        zh => <<"无效性别: "/?ENCODING>>,
        en => <<"Invalid sex: ">>
    },

    please_input_born_month => #{
        zh => <<"请输入角色的出生月份:\n(1 - 12)"/?ENCODING>>,
        en => <<"Please input born month:\n(1 - 12)">>
    },

    invalid_month => #{
        zh => <<"无效月份: "/?ENCODING>>,
        en => <<"Invalid born month: ">>
    },

    is_confirmed => #{
        zh => <<"确定吗? (y / n)"/utf8>>,
        en => <<"Confirm? (y / n)">>
    },

    welcome_join => #{
        zh => <<"欢迎加入"/?ENCODING>>,
        en => <<"Welcome join">>
    },

    male => #{
        zh => <<"男"/?ENCODING>>,
        en => <<"Male">>
    },

    female => #{
        zh => <<"女"/?ENCODING>>,
        en => <<"Female">>
    }

})).