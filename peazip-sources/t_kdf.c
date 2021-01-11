#include <tomcrypt.h>

/*
 * KDF test vector calculation program, W.Ehrhardt Jul 2008
 * Calculates TVs for pkcs_5_alg1/2 with md5, sha1, sha224/256/384/512, whirlpool
 * Data from RFC 3211 - Password-based Encryption for CMS
 * sha1 output matches RFC 3211
 */


void usage(char* name)
{
  printf("Usage: %s <hash>\n", name);
  printf(" hash: md4 | md5 | rmd160 | sha1 | sha224 | sha256 | sha384 | sha512 | whirlpool\n");
  printf(" calculates pbkdf1/2 and mgf1 for specified hash\n");
}

int main(int argc, char* argv[])
{
  unsigned char pwd1[8]  = "password";
  unsigned char salt1[8] = {0x12, 0x34, 0x56, 0x78, 0x78, 0x56, 0x34, 0x12};

  unsigned char pwd2[76] = "All n-entities must communicate with other n-entities via n-1 entiteeheehees";
  unsigned char salt2[8] = {0x12, 0x34, 0x56, 0x78, 0x78, 0x56, 0x34, 0x12};

  unsigned char outbuf[48];
  int err, hash;
  unsigned long i,outlen;

  if (argc<2) {
    usage(argv[0]);
    return -1;
  }


  if (!strcmp(argv[1],"md5")) {
    register_hash(&md5_desc);
  }
  else if (!strcmp(argv[1],"md4")) {
    register_hash(&md4_desc);
  }
  else if (!strcmp(argv[1],"sha1")) {
    register_hash(&sha1_desc);
  }
  else if (!strcmp(argv[1],"rmd160")) {
    register_hash(&rmd160_desc);
  }
  else if (!strcmp(argv[1],"sha224")) {
    register_hash(&sha224_desc);
  }
  else if (!strcmp(argv[1],"sha256")) {
    register_hash(&sha256_desc);
  }
  else if (!strcmp(argv[1],"sha384")) {
    register_hash(&sha384_desc);
  }
  else if (!strcmp(argv[1],"sha512")) {
    register_hash(&sha512_desc);
  }
  else if (!strcmp(argv[1],"whirlpool")) {
    register_hash(&whirlpool_desc);
  }
  else {
    usage(argv[0]);
    return -1;
  }

  if ((hash = find_hash(argv[1])) < 0) {
    printf("find_hash error: %s\n", argv[1]);
    return -1;
  }

  printf("pkcs_5_alg2:\n\n");

  outlen = 8;
  if ((err = pkcs_5_alg2(pwd1, 8, salt1, 8, 5, hash, outbuf, &outlen)) != CRYPT_OK) {
    printf("pkcs_5_alg2 error!\n");
    return -1;
  }
  printf(";Test 1 %s\n", argv[1]);
  for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
  printf("\n\n");

  outlen = 16;
  if ((err = pkcs_5_alg2(pwd2, 76, salt2, 8, 500, hash, outbuf, &outlen)) != CRYPT_OK) {
    printf("pkcs_5_alg2 error!\n");
    return -1;
  }
  printf(";Test 2 %s\n", argv[1]);
  for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
  printf("\n\n");

  outlen = 16;
  if ((err = pkcs_5_alg2(pwd2, 76, salt2, 8, 300000, hash, outbuf, &outlen)) != CRYPT_OK) {
    printf("pkcs_5_alg2 error!\n");
    return -1;
  }
  printf(";Test 3 %s\n", argv[1]);
  for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
  printf("\n\n");


  printf("pkcs_5_alg1:\n\n");

  outlen = 8;
  if ((err = pkcs_5_alg1(pwd1, 8, salt1, 5, hash, outbuf, &outlen)) != CRYPT_OK) {
    printf("pkcs_5_alg1 error!\n");
    return -1;
  }
  printf(";Test 4 %s\n", argv[1]);
  for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
  printf("\n\n");

  if (hash_descriptor[hash].hashsize>15) {
    outlen = 16;
    if ((err = pkcs_5_alg1(pwd2, 76, salt2, 500, hash, outbuf, &outlen)) != CRYPT_OK) {
      printf("pkcs_5_alg1 error!\n");
      return -1;
    }
    printf(";Test 5 %s\n", argv[1]);
    for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
    printf("\n\n");

    outlen = 16;
    if ((err = pkcs_5_alg1(pwd2, 76, salt2, 300000, hash, outbuf, &outlen)) != CRYPT_OK) {
      printf("pkcs_5_alg1 error!\n");
      return -1;
    }
    printf(";Test 6 %s\n", argv[1]);
    for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
    printf("\n\n");
  }
  return 0;

  printf("pkcs_1_mgf1:\n\n");

  outlen = 24;
  if ((err = pkcs_1_mgf1(hash, pwd1, 8, outbuf, outlen)) != CRYPT_OK) {
    printf("pkcs_1_mgf1 error!\n");
    return -1;
  }
  printf(";Test 7 %s\n", argv[1]);
  for (i=0; i<outlen; i++) {printf("%02x",outbuf[i]);}
  printf("\n\n");

  return 0;

}
