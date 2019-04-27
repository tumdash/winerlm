winerlm
=====

A stateless library for doing WinRM.

Examples
-----

```erlang
1> Cred = #{user => <<"myUser">>, password =><<"MyPassOnWin123">>}.
#{password => <<"MyPassOnWin123">>,user => <<"myUser">>}
2> winerlm:get_cim_instance(<<"2XX.XXX.XXX.XXX">>, Cred, <<"Win32_OperatingSystem">>, void).
{ok,#{<<"RegisteredUser">> => <<"wisr">>,
      <<"DataExecutionPrevention_SupportPolicy">> => 3,
      <<"ServicePackMajorVersion">> => 0,
      <<"OSArchitecture">> => <<"64-bit">>,
      <<"WindowsDirectory">> => <<"C:\\Windows">>,
      <<"TotalVirtualMemorySize">> => 3188,
      <<"OSLanguage">> => 1033,
      <<"TotalVisibleMemorySize">> => 2096,
      <<"Status">> => <<"OK">>,<<"CSName">> => <<"MyWinName">>,
      <<"BootDevice">> => <<"\\Device\\HarddiskVolume3">>,
      <<"Locale">> => 409,<<"Debug">> => false,
      <<"CountryCode">> => 1,
      <<"SerialNumber">> => <<"22222-777777-00000-AAAAA">>,
      <<"ProductType">> => 3,<<"BuildNumber">> => 9600,
      <<"Organization">> => <<"myOrg.com">>,
      <<"CSCreationClassName">> => <<"Win32_ComputerSystem">>,
      <<"SuiteMask">> => 282,
      <<"SystemDirectory">> => <<"C:\\Windows\\system32">>,
      <<"CodeSet">> => 1242,
      <<"Caption">> => <<"Microsoft Window"...>>,
      <<"MUILanguages">> => <<"en-US">>,<<"Primary">> => true,
      <<"Oper"...>> => 7,<<...>> => 1002620,...}}
3>
4> winerlm:get_cim_instance(<<"2XX.XXX.XXX.XXX">>, Cred, <<"Win32_Service">>, void).
{error,<<"The WS-Management service cannot process the request. The WMI service returned an 'access denied' error. ">>}
```

Build
-----

    $ rebar3 get-deps compile
