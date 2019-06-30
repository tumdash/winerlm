-module(winerlm_ntlm).

-export([authenticate_v2/3, negotiate_v2/1]).

% length of the fixed-size fields before payload
-define(NEG_OFFSET, 40).
-define(AUTH_OFFSET, 72).

%%%=============================================================================
%%% API
%%%=============================================================================
% 3.1.5.1.1 [MS-NLMP]
negotiate_v2(Options) ->
    Workstation = maps:get(workstation, Options, <<"workstation">>),
    Domain = maps:get(domain, Options, <<"domain">>),
    [DomainF, WorkstationF] = prepare_fields([Domain, Workstation], ?NEG_OFFSET),
    <<
        "NTLMSSP", 0, 1:32/little, % NEGOTIATE_MESSAGE
        (<<50,176,136,226>>)/binary,        % negotiation flag
        DomainF/binary,
        WorkstationF/binary,
        (<<6,1,177,29,0,0,0,15>>)/binary,   % OS ver
        Domain/binary,
        Workstation/binary
    >>.

% 3.1.5.1.2 and 3.3.2 [MS-NLMP]
authenticate_v2(Options, NegotiateMsg, ChallengeMsg) ->
    % next does NOT support anonymous authentication,
    % so user and password should be present always
    User = maps:get(user, Options),
    Password = maps:get(password, Options),
    Workstation = maps:get(workstation, Options, <<"workstation">>),
    Domain = maps:get(domain, Options, <<"domain">>),
    ResponseKeyNT = ntowfv2(Password, User, Domain),
    {Timestamp, ServerChallenge, TargetInfoBlock} = parse_challenge(ChallengeMsg),
    TempBlob = ntlmv2_blob(Timestamp, TargetInfoBlock),
    NTProofStr = hmac_md5(ResponseKeyNT, <<ServerChallenge/binary, TempBlob/binary>>),
    NtChallengeResponse = <<NTProofStr/binary, TempBlob/binary>>,
    % 3.1.5.1.1: If LM authentication is not being used, then the client
    % sets the NTLMSSP_NEGOTIATE_EXTENDED_SESSIONSECURITY flag in the NegotiateFlags
    LmChallengeResponse = z(24),
    SessionBaseKey = hmac_md5(ResponseKeyNT, NTProofStr),
    KeyExchangeKey = kxkey(SessionBaseKey, LmChallengeResponse, ServerChallenge),
    EncryptedRandomSessionKey = rc4k(KeyExchangeKey, ExportedSessionKey = nonce(16)),
    NoMICAuthMsg = auth_msg(
        LmChallengeResponse,
        NtChallengeResponse,
        Domain,
        User,
        Workstation,
        EncryptedRandomSessionKey,
        _EmptyMIC = z(16)
    ),
    auth_msg(
        LmChallengeResponse,
        NtChallengeResponse,
        Domain,
        User,
        Workstation,
        EncryptedRandomSessionKey,
        mic(ExportedSessionKey, NegotiateMsg, ChallengeMsg, NoMICAuthMsg)
    ).

%%%=============================================================================
%%% Helpers
%%%=============================================================================
prepare_fields(FieldsList, BasicOffset) ->
    {Fields, _} = lists:mapfoldl(
        fun(Element, Offset) ->
            FieldLen = byte_size(Element),
            Field = <<FieldLen:16/little, FieldLen:16/little, Offset:32/little>>,
            {Field, Offset + FieldLen}
        end,
        BasicOffset,
        FieldsList
    ),
    Fields.

auth_msg(LmResp, NtResp, Domain, User, Workstation, SessionKey, MIC) ->
    [DomainF, UserF, WorkstationF, LmRespF, NtRespF, SessionKeyF] = prepare_fields(
            [Domain, User, Workstation, LmResp, NtResp, SessionKey],
            ?AUTH_OFFSET + byte_size(MIC)
    ),
    <<
        "NTLMSSP", 0, 3:32/little, % AUTHENTICATE_MESSAGE with Signature
        LmRespF/binary,
        NtRespF/binary,
        DomainF/binary,
        UserF/binary,
        WorkstationF/binary,
        SessionKeyF/binary,
        (<<54,130,138,226>>)/binary,         % negotiation flag
        (<<6,1,177,29,0,0,0,15>>)/binary,    % OS ver
        MIC/binary,
        Domain/binary,
        User/binary,
        Workstation/binary,
        LmResp/binary,
        NtResp/binary,
        SessionKey/binary
    >>.

% Set temp to ConcatenationOf(Responserversion,
% HiResponserversion, Z(6), Time, ClientChallenge, Z(4),
% ServerName, Z(4))
% NOTE: In fact not ServerName but TargetInfo block from ChallengeMsg is used
ntlmv2_blob(Timestamp, TargetInfoBlock) ->
    ClientNonce = nonce(8),
    <<
        1:8, 1:8, 0:16,     % blob signature
        0:32,               % reserved zeroes
        Timestamp/binary,   % timestamp from av payload
        ClientNonce/binary, % random client nonce
        0:32,               % reserved zeroes
        TargetInfoBlock/binary,
        0:32                % reserved zeroes
    >>.

% CHALLENGE_MESSAGE signature
parse_challenge(<<"NTLMSSP", 0, 2:32/little, RestMsg/binary>>) ->
    <<
      % target name field
        TNameLen:16/little, _TNameLenMax:16/little, _TNameOffset:32/little,
        _NegotiationFlags:32/little,
        ServerChallenge:64/little,
        0:64/little, % reserved but should be zeroes
        % target info field
        _TInfoLen:16/little, _TInfoLenMax:16/little, _TInfoOffset:32/little,
        _StationVer:64/little,
        VariablePayload/binary
    >> = RestMsg,
    % parse payload by given lengths
    TNameSize = 8*TNameLen,
    %_TInfoSize = 8*TInfoLen,
    %io:format("TNameSize: ~p TInfoSize: ~p~n", [TNameSize, TInfoSize]),
    <<_TargetName:TNameSize/little, InfoBlock/binary>> = VariablePayload,
    AVMap = parse_av_list(InfoBlock, #{}),
    {maps:get(msvAvTimestamp, AVMap), <<ServerChallenge:64/little>>, output_av_list(InfoBlock)}.

%MsvAvEOL
parse_av_list(<<0:32>>, Map) ->
    Map;
%MsvAvEOL
parse_av_list(<<AvType:16/little, Len:16/little, RestAV/binary>>, Accum) ->
    NewAccum = maps:put(av_key(AvType), binary:part(RestAV, 0, Len), Accum),
    NextAv = binary:part(RestAV, Len, byte_size(RestAV) - Len),
    parse_av_list(NextAv, NewAccum).

av_key(1) -> msvAvNbComputerName;
av_key(2) -> msvAvNbDomainName;
av_key(3) -> msvAvDnsComputerName;
av_key(4) -> msvAvDnsDomainName;
av_key(5) -> msvAvDnsTreeName;
av_key(6) -> msvAvFlags;
av_key(7) -> msvAvTimestamp;
av_key(8) -> msvAvSingleHost;
av_key(9) -> msvAvTargetName;
av_key(10) -> msvChannelBindings.

output_av_list(InfoBlock) ->
    ServerInfoBlock = binary:part(InfoBlock, 0, byte_size(InfoBlock)-4),
    <<
        ServerInfoBlock/binary,
        % 3.1.5.2.1 If the CHALLENGE_MESSAGE TargetInfo field has an MsvAvTimestamp present,
        % the client SHOULD provide a MIC<56>:
        % add/modify an AV_PAIR structure and set the AvId field to MsvAvFlags
        % and the Value field bit 0x2 to 1.
        6:16/little, 4:16/little, 2:32/little,
        % MsvAvEOL
        0:32
    >>.

% 3.1.5.1.2 Set MIC to HMAC_MD5(ExportedSessionKey,
% ConcatenationOf( NEGOTIATE_MESSAGE, CHALLENGE_MESSAGE, AUTHENTICATE_MESSAGE))
mic(Key, NegotiateMsg, ChallengeMsg, NoMICAuth) ->
    hmac_md5(Key, <<NegotiateMsg/binary, ChallengeMsg/binary, NoMICAuth/binary>>).

% 3.4.5.1 If NTLM v2 is used, KeyExchangeKey MUST be set to
% the given 128-bit SessionBaseKey value.
kxkey(SessionBaseKey, _LmChallengeResponse, _ServerChallenge) ->
    SessionBaseKey.

% The encryption of data item with the key using the RC4 algorithm.
rc4k(KeyExchangeKey, ExportedSessionKey) ->
    case list_to_integer(erlang:system_info(otp_release)) of
        BuiltinR when BuiltinR >= 22 ->
            crypto:crypto_one_time(rc4, KeyExchangeKey, ExportedSessionKey, _Encrypt = true);
        _NoBuiltinR ->
            winerlm_crypto:rc4(KeyExchangeKey, ExportedSessionKey)
    end.

% NTLMv2 [3.3.2] of [MS-NLMP]
% Define NTOWFv2(Passwd, User, UserDom) as
% HMAC_MD5( MD4(UNICODE(Passwd)), UNICODE(ConcatenationOf( Uppercase(User), UserDom ) ) )
ntowfv2(Password, User, Domain) ->
    NtlmHash = md4(unicode(Password)),
    UserBin = list_to_binary(uppercase(binary_to_list(User))),
    UserDomainBin = unicode(<<UserBin/binary, Domain/binary>>),
    hmac_md5(NtlmHash, UserDomainBin).

% MD4 message digest of the null-terminated byte string M
md4(M) ->
    crypto:hash(md4, M).

% HMAC_MD5 message digest of string Message based on cipherkey Key
hmac_md5(Key, Message) ->
    crypto:hmac(md5, Key, Message).

% 2-byte little-endian encoding of the Unicode UTF-16 representation of string.
unicode(String) ->
    unicode:characters_to_binary(String, utf8, {utf16, little}).

% Uppercase representation of string.
uppercase(String) ->
    string:to_upper(String).

% Creation of a byte array of length N, where each byte is initialized to zero.
z(N) ->
    <<0:(N*8)>>.

nonce(N) when is_number(N)->
    % cryptographically strong random number generator
    crypto:strong_rand_bytes(N).

% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
% a little magic from http://stackoverflow.com/users/2760050/himangshuj
binary_to_hex(Id) ->
    << <<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X,16)>>.

hex_to_binary(Id) ->
    <<<<Z>> || <<X:8,Y:8>> <= Id,Z <- [binary_to_integer(<<X,Y>>,16)]>>.
