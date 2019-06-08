class InternalException(Exception):
    def __str__(self):
        return type(self).__name__


class UserIsFrozenError(InternalException):
    pass


class DoesNotExist(InternalException):
    pass


class InvalidInputError(InternalException):
    pass


class IncorrectCredentials(InternalException):
    pass


class CommandNotSupported(InternalException):
    pass


class Forbidden(InternalException):
    pass
