import unittest

from application.lib.parser import parseFromFile
from application.lib.transform_to_dka import transformation


class TestTransformation(unittest.TestCase):

    def test(self):
        nka = parseFromFile('nka.txt')
        dka_transform = transformation(nka)
        self.assertEqual('q0', dka_transform.root.name)
        self.assertEqual(['a', 'b'], dka_transform.alfabet)
        self.assertEqual(2, len(dka_transform.states))
        self.assertEqual("q0_q1", dka_transform.states['q0'].go('a').name)
        self.assertEqual("q0", dka_transform.states['q0'].go('b').name)
        self.assertEqual("q0_q1", dka_transform.states['q0_q1'].go('a').name)
        self.assertEqual("q0_q1", dka_transform.states['q0_q1'].go('b').name)





if __name__ == '__main__':
    unittest.main()