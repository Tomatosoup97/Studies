import hashlib
import binascii
import os

HASHING_ALG = 'sha512'
SALT_LEN = 64
ITERATIONS = 100000


def hash_password(password: str) -> str:
    salt = hashlib.sha256(os.urandom(60)).hexdigest().encode('ascii')
    pwdhash = hashlib.pbkdf2_hmac(HASHING_ALG,
                                  password.encode('utf-8'),
                                  salt, ITERATIONS)
    pwdhash = binascii.hexlify(pwdhash)
    return (salt + pwdhash).decode('ascii')


def verify_password(stored_password: str, provided_password: str) -> bool:
    salt = stored_password[:SALT_LEN]
    stored_password = stored_password[SALT_LEN:]
    pwdhashb = hashlib.pbkdf2_hmac(HASHING_ALG,
                                   provided_password.encode('utf-8'),
                                   salt.encode('ascii'), ITERATIONS)
    pwdhash = binascii.hexlify(pwdhashb).decode('ascii')
    return pwdhash == stored_password
