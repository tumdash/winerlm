%%%=============================================================================
%%% Module for work with winrm XML
%%%=============================================================================
-module(winerlm_xml).

-include_lib("xmerl/include/xmerl.hrl").

%% API exports
-export([
		enumerate/3,
		pull/5,
		parse_response/1
]).

%%%=============================================================================
%% API functions
%%%=============================================================================
enumerate(ComputerName, ClassName, Uuid) ->
		<<
		"<s:Envelope "
			"xmlns:s=\"http://www.w3.org/2003/05/soap-envelope\" "
			"xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\" "
			"xmlns:wsman=\"http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd\" "
			"xmlns:wsen=\"http://schemas.xmlsoap.org/ws/2004/09/enumeration\">"
				"<s:Header>"
					"<wsa:To>http://", ComputerName/binary, ":5985/wsman</wsa:To>"
						"<wsman:ResourceURI s:mustUnderstand=\"true\">"
"http://schemas.microsoft.com/wbem/wsman/1/wmi/root/cimv2/", ClassName/binary,
						"</wsman:ResourceURI>"
					"<wsa:ReplyTo>"
						"<wsa:Address s:mustUnderstand=\"true\">"
"http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous"
						"</wsa:Address>"
					"</wsa:ReplyTo>"
					"<wsa:Action s:mustUnderstand=\"true\">"
"http://schemas.xmlsoap.org/ws/2004/09/enumeration/Enumerate"
					"</wsa:Action>"
					"<wsman:MaxEnvelopeSize s:mustUnderstand=\"true\">"
						"51200"
					"</wsman:MaxEnvelopeSize>"
					"<wsa:MessageID>uuid:", Uuid/binary, "</wsa:MessageID>"
	         "<wsman:OperationTimeout>PT60.000S</wsman:OperationTimeout>"
				"</s:Header>"
	     "<s:Body><wsen:Enumerate/></s:Body>"
		"</s:Envelope>">>.

pull(ComputerName, ClassName, Context, MaxNum, Uuid) ->
		MaxNumBin = integer_to_binary(MaxNum),
		<<
		"<s:Envelope "
			"xmlns:s=\"http://www.w3.org/2003/05/soap-envelope\" "
			"xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\" "
			"xmlns:wsman=\"http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd\" "
			"xmlns:wsen=\"http://schemas.xmlsoap.org/ws/2004/09/enumeration\">"
			"<s:Header>"
				"<wsa:To>http://", ComputerName/binary, ":5985/wsman</wsa:To>"
	         	"<wsman:ResourceURI s:mustUnderstand=\"true\">"
"http://schemas.microsoft.com/wbem/wsman/1/wmi/root/cimv2/", ClassName/binary,
				"</wsman:ResourceURI>"
					"<wsa:ReplyTo>"
						"<wsa:Address s:mustUnderstand=\"true\">"
"http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous"
						"</wsa:Address>"
					"</wsa:ReplyTo>"
				"<wsa:Action s:mustUnderstand=\"true\">"
"http://schemas.xmlsoap.org/ws/2004/09/enumeration/Pull"
				"</wsa:Action>"
				"<wsman:MaxEnvelopeSize s:mustUnderstand=\"true\">"
					"51200"
				"</wsman:MaxEnvelopeSize>"
				"<wsa:MessageID>uuid:", Uuid/binary, "</wsa:MessageID>"
				"<wsman:OperationTimeout>PT60.000S</wsman:OperationTimeout>"
			"</s:Header>"
			"<s:Body>"
				"<wsen:Pull>"
					"<wsen:EnumerationContext>uuid:",
						Context/binary,
					"</wsen:EnumerationContext>"
					"<wsen:MaxElements>", MaxNumBin/binary, "</wsen:MaxElements>"
				"</wsen:Pull>"
			"</s:Body>"
		"</s:Envelope>"
		>>.

parse_response(XmlResponse) ->
		Xml = try_scan_xml(binary_to_list(XmlResponse)),
		[SoapMsg] = xmerl_xpath:string("//s:Envelope/s:Body/node()", Xml),
		%io:format("BodyParse: ~p~n", [BodyParse]),
		parse_type(SoapMsg, XmlResponse).

%%%=============================================================================
%% Internal functions
%%%=============================================================================
parse_type(#xmlElement{name='s:Fault'} = Xml, _XmlResponse) ->
		[#xmlText{value=TextError}] =
			xmerl_xpath:string("//s:Reason/s:Text/text()", Xml),
		{failure, list_to_binary(TextError)};
parse_type(#xmlElement{name='n:EnumerateResponse'} = Xml, _XmlResponse) ->
		[#xmlText{value="uuid:" ++ Uuid}] =
			xmerl_xpath:string("//n:EnumerateResponse/n:EnumerationContext/text()", Xml),
		{enumeration_resp, list_to_binary(Uuid)};
parse_type(#xmlElement{name='n:PullResponse'} = Xml, _XmlResponse) ->
		%TODO in general case that is fair list, not only 1 element list
		[#xmlElement{name=ItemName} = PullElement] =
			xmerl_xpath:string("//n:PullResponse/n:Items/node()", Xml),
		XPathElem = "//" ++ atom_to_list(ItemName) ++ "/node()",
		Properties = xmerl_xpath:string(XPathElem, PullElement),
		FinalMap = lists:foldl(fun collect_property/2, #{}, Properties),
		{pull_resp, FinalMap};
parse_type(#xmlElement{name=Unknown} = _Xml, _XmlResponse) ->
		{failure, Unknown}.

collect_property(#xmlElement{nsinfo={_, Local}, name=Name} = Elem, AccMap) ->
		XPathElem = "//" ++ atom_to_list(Name) ++ "/text()",
		case xmerl_xpath:string(XPathElem, Elem) of
			[#xmlText{value=Value}] ->
				maps:merge(#{list_to_binary(Local) => handle_value(Value)}, AccMap);
			[] ->
				AccMap
		end.

try_scan_xml("") ->
		{failure, empty_response};
try_scan_xml(XmlBinary) ->
		try xmerl_scan:string(XmlBinary) of
				{Xml, _} ->
						Xml
		catch
				_:R ->
						{failure, {scan_xml_error, XmlBinary, R}}
		end.

handle_value("true") ->
		true;
handle_value("false") ->
		false;
handle_value(PossibleInt) ->
		try list_to_integer(PossibleInt) of
			Int ->
					Int
		catch
			_:_ ->
					list_to_binary(PossibleInt)
		end.
