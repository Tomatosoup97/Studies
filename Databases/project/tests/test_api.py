import api
from mytypes import SQLQuery


class TestVote:
    def test_upvote(self):
        params = {
            'timestamp': 1, 'member': 1, 'password': 'passwd', 'action': 1,
        }
        api.upvote(**params)

    def test_vote(self):
        [(SQLQuery('', {}), lambda i: 1)]
