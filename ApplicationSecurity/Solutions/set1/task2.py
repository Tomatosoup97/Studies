import timeit
import random

from Crypto import Random
from Crypto.Cipher import PKCS1_OAEP
from Crypto.PublicKey import RSA
from cryptography.fernet import Fernet


class SymmetricEncryption(object):
    """
    Symmetric encryption using secret key.
    Internal implementation is built on top of AES and CBC.
    """
    def __init__(self, secret_key, msg):
        self.secret_key = secret_key
        self.msg = msg

    def __call__(self):
        return self.encrypt()

    def encrypt(self):
        f = Fernet(self.secret_key)
        ciphertext = f.encrypt(bytes(self.msg))
        return ciphertext

    @staticmethod
    def gen_key(k=1):
        return Fernet.generate_key()


class AsymmetricEncryption(object):
    """Asymmetric encryption, OAEP standard.

    Based on RSA encryption algorithm, see RFC 3447 for more details
    on specification http://www.ietf.org/rfc/rfc3447.txt
    """
    def __init__(self, key, msg):
        self.key = key
        self.msg = bytes(msg)

    def __call__(self):
        return self.encrypt()

    def encrypt(self):
        publickey = self.key.publickey()
        ciphertext = publickey.encrypt(self.msg, random.randint(1, 100))
        return ciphertext

    @staticmethod
    def gen_key(key=2048):
        random_generator = Random.new().read
        key = RSA.generate(key, random_generator)
        return key


class EncryptionReport(object):
    """
    Compare two encryption techniques
    """
    def _get_message(self, k=1):
        return "Top secret information " * k

    def _EncryptorFactory(self, encryptor_class, k=1, key_len=1024):
        return encryptor_class(encryptor_class.gen_key(key_len),
                               self._get_message(k))

    def report_results(self):
        print("-- Encryption results --")
        print("Symmetric encryption: ")
        encryptor = self._EncryptorFactory(SymmetricEncryption)
        print(encryptor.encrypt())

        print("\n")

        print("Asymmetric encryption: ")
        encryptor = self._EncryptorFactory(AsymmetricEncryption)
        print(encryptor.encrypt())
        print("-----\n")

    def measure_performance(self):
        ITERATIONS = 1000

        print("-- Performance results --")
        print("{} iterations\n".format(ITERATIONS))

        print("Symmetric encryption: \n")
        print("- 23 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(SymmetricEncryption).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("- 230 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(SymmetricEncryption, 10).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("\n")

        print("Asymmetric encryption: \n")

        print("\n--- 1024 key: --- \n")

        print("- 23 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(AsymmetricEncryption, 1).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("- 115 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(AsymmetricEncryption, 5).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("\n--- 4096 key: --- \n")

        print("- 23 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(AsymmetricEncryption, 1, 4096).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("- 230 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(AsymmetricEncryption, 10, 4096).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("- 460 chars blocktext: ")
        t = timeit.Timer(self._EncryptorFactory(AsymmetricEncryption, 20, 4096).encrypt)
        print(t.timeit(number=ITERATIONS))

        print("-----\n")

    def report(self):
        self.report_results()
        self.measure_performance()


if __name__ == '__main__':
    reporter = EncryptionReport()
    reporter.report()
