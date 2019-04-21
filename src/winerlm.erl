%%%=============================================================================
%%% Library for Windows Remote Management
%%%=============================================================================
-module(winerlm).

%% API exports
-export([
    get_cim_instance/4,
    enumerate_cim_instances/3
]).

%%%=============================================================================
%% API functions
%%%=============================================================================
%% Reads specified CIM ClassName with specified Selectors from remote or local
%% CIM-based system i.e.
%%  get_cim_instance(
%%      <<"127.0.0.1">>,
%%      #{domain => <<"MyDomain">>, user => <<"username">>, password => <<"passwd">>},
%%      <<"Win32_Service">>,
%%      #{<<"Name">> => <<"spooler">>}
%%  ) returns {ok, Data} where
%%    Data = #{
%%        <<"AcceptPause">> => false,
%%        ...
%%        <<"ExitCode">> => 0,
%%        ...
%%        <<"SystemCreationClassName">> => <<"Win32_ComputerSystem">>,
%%        ...
%%    }
%%
-spec get_cim_instance(
        ComputerName    :: binary(),
        Options         :: map(),
        ClassName       :: binary(),
        KeyIdentity     :: map()
) ->
    {ok, CIMData :: map()} | {error, Reason :: term()}.
get_cim_instance(ComputerName, Options, ClassName, KeyIdentity) ->
    {error, not_implemented}.

%% Filters specified CIM ClassNames with specified WQL from remote or local
%% CIM-based system i.e.
%%  enumerate_cim_instances(
%%      <<"127.0.0.1">>,
%%      #{domain => <<"MyDomain">>, user => <<"UserName">>, password => <<"passwd">>},
%%      <<"select * FROM Win32_NetworkAdapterConfiguration WHERE IpEnabled = TRUE">>,
%%  ) returns {ok, Data} where
%%    Data = [
%%      #{
%%        <<"DHCPEnabled">> => true,
%%        <<"IpAddress">> => <<"{192.168.1.33, fe80::7dec:0475:11d7:1376}">>
%%        <<"Index">> => 7,
%%        <<"serviceName">> => <<"someApp1">>
%%        ...
%%      },
%%      #{
%%        <<"DHCPEnabled">> => false,
%%        <<"IpAddress">> => <<"{10.0.1.42, 1::8899:1001:11d7:1376}">>
%%        <<"Index">> => 32,
%%        <<"serviceName">> => <<"someApp2">>,
%%        ...
%%      }
%%    ]
%%
-spec enumerate_cim_instances(
        ComputerName    :: binary(),
        Options         :: map(),
        Wql             :: binary()
) ->
    {ok, CIMData :: [map()]} | {error, Reason :: term()}.
enumerate_cim_instances(ComputerName, Options, Wql) ->
    {error, not_implemented}.

%%%=============================================================================
%% Internal functions
%%%=============================================================================
