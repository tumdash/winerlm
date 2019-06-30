#include "erl_nif.h"
#include "string.h"

static ERL_NIF_TERM rc4_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i, j, c;
    ErlNifBinary res_bin, key_bin, text_bin;
    unsigned char s[256], temp;

/* Read input parameters */
    if (!enif_inspect_binary(env, argv[0], &key_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &text_bin))
        return enif_make_badarg(env);

/* RC4_init */
    for(i = 0; i < 256; ++i)
      s[i] = i;

    j = 0;
    for(i = 0; i < 256; ++i)
    {
        j = (j + key_bin.data[i % key_bin.size] + s[i]) % 256;
        temp = s[i];
        s[i] = s[j];
        s[j] = temp;
    }

/* allocate result binary */
    if (!enif_alloc_binary(text_bin.size, &res_bin))
        return enif_make_badarg(env);

/* RC4_output and XOR with text */
    i = j = 0;
    for(c = 0; c < text_bin.size; ++c)
    {
        i = (i + 1) % 256;
        j = (j + s[i]) % 256;
        temp = s[j];
        s[j] = s[i];
        s[i] = temp;
        res_bin.data[c] = text_bin.data[c] ^ s[(temp + s[j]) % 256];
    }

    return enif_make_binary(env, &res_bin);
}

static ErlNifFunc nif_funcs[] = {
    {"rc4", 2, rc4_nif}
};

ERL_NIF_INIT(winerlm_crypto, nif_funcs, NULL, NULL, NULL, NULL)
