import base64
import sys
from ntlm_auth.ntlm import NtlmContext

class NtlmAuth(NtlmContext):
    def negotiate_msg(self):
        ntlm_context = NtlmContext(self.username, self.password, self.domain, self.workstation, ntlm_compatibility=5)
        negotiate_message = ntlm_context.step()
        return base64.b64encode(negotiate_message)

    def auth_msg(self, challenge_message):
        ntlm_context = NtlmContext(self.username, self.password, self.domain, self.workstation, ntlm_compatibility=5)
        negotiate_message = ntlm_context.step()
        authenticate_message = ntlm_context.step(challenge_message)
        return base64.b64encode(authenticate_message)

if __name__ =='__main__':
    try:
        ntlm_auth = NtlmAuth(
            username=sys.argv[2],
            password=sys.argv[3],
            domain=sys.argv[4],
            workstation=sys.argv[5],
            ntlm_compatibility=5
        )
        command = sys.argv[1]
        if command == 'negotiate':
            print(ntlm_auth.negotiate_msg())
        elif command =='authenticate':
            print(ntlm_auth.auth_msg(base64.b64decode(sys.argv[6])))
    except IndexError:
        print """wrong parameters"""
