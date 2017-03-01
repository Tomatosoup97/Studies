import timeit
import hashlib
import argon2
import bcrypt


password = b'x=cub=VHutdD$nxqM6$YSwR$$KtxHFhAzw%Sdvz4v8z6cnJ2b$' * 100
salt = b'$2b$12$DhLWuXOtW5ga8oKLO6cIqe'

iterations = 10


def md5_test():
    m = hashlib.md5()
    m.update(password + salt)


def bcrypt_test():
    bcrypt.hashpw(password, salt)


def sha256_test():
    hashlib.sha256(password + salt)


def argon_test():
    argon2.argon2_hash(password=password, salt=salt)


def measure_md5():
    print('## MD5: ')
    print('{0:.15f}'.format(
        timeit.timeit('md5_test()',
                      setup="from __main__ import md5_test",
                      number=iterations), 10))
    print('\n')


def measure_argon():
    print('## Argon: ')
    print(timeit.timeit('argon_test()',
                        setup="from __main__ import argon_test",
                        number=iterations))
    print('\n')


def measure_sha256():
    print('## SHA256: ')
    print(timeit.timeit('sha256_test()',
                        setup="from __main__ import sha256_test",
                        number=iterations))
    print('\n')


def measure_bcrypt():
    print('## Bcrypt: ')
    print(timeit.timeit('bcrypt_test()',
                        setup="from __main__ import bcrypt_test",
                        number=iterations))
    print('\n')


def measure():
    print('# Hash functions performance measurement \n')
    print('{} iterations\n'.format(iterations))
    print('{} char text\n'.format(len(password + salt)))
    print('---')
    measure_md5()
    measure_sha256()
    measure_argon()
    measure_bcrypt()


if __name__ == '__main__':
    measure()
