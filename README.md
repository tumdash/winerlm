winerlm
=====

A stateless library for doing WinRM.

Examples
-----
library uses [hackney](https://github.com/benoitc/hackney) as http client,
so before doing any examples this need to be started e.g.

```erlang
1> application:ensure_all_started(hackney).
{ok,[unicode_util_compat,idna,mimerl,certifi,ssl_verify_fun,
     metrics,hackney]}
```

## Basic Auth

```erlang
2> Cred = #{user => <<"myUser">>, password =><<"MyPassOnWin123">>}.
#{password => <<"MyPassOnWin123">>,user => <<"myUser">>}
3> winerlm:get_cim_instance(<<"2XX.XXX.XXX.XXX">>, Cred, <<"Win32_OperatingSystem">>, void).
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
4>
5> winerlm:get_cim_instance(<<"2XX.XXX.XXX.XXX">>, Cred, <<"Win32_Service">>, void).
{error,<<"The WS-Management service cannot process the request. The WMI service returned an 'access denied' error. ">>}
```

## NTLM Auth

Unfortunately there is [no](https://github.com/gotthardp/erlang-ntlm) erlang library
for NTLM Authentication, so Python [NTLM library](https://github.com/jborean93/ntlm-auth)
is used for generating security NTLM headers.

So before starting using NTLM, its required this specific python library to be installed,
so `etc/auth_script.py` could be successfully called from erlang.

```erlang
2> Cred = #{user => <<"myUser">>, password =><<"MyPassOnWin123">>, transport => ntlm}.
#{password => <<"MyPassOnWin123">>,transport => ntlm, user => <<"myUser">>}
3> winerlm:get_cim_instance(<<"3XX.XXX.XXX.XXX">>, Cred, <<"Win32_OperatingSystem">>, void).
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
```

Build & Play
-----

```sh
$ rebar3 get-deps
$ rebar3 compile
$ rebar3 shell
```
