-module(winerlm_crypto).
-export([rc4/2]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "winerlm"), 0).

rc4(_Key, _Text) ->
    exit(nif_library_not_loaded).
