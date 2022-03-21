import unittest

from application.lib.parser import parseFromFile
from application.lib.minimize_dka import minimize


class TestTransformation(unittest.TestCase):

    def test(self):
        dka_transform = minimize(parseFromFile('for_min'))
        self.assertEqual('q0q1q3q4', dka_transform.root.name)
        final_states = [name for name in dka_transform.name_of_states if dka_transform.states[name].is_terminal]
        self.assertEqual(len(final_states), 1)
        self.assertTrue("q2q5" in final_states)
        self.assertEqual(['a', 'b'], dka_transform.alfabet)
        self.assertEqual(2, len(dka_transform.states))
        transitions = list(dka_transform.transitions.items())
        self.assertTrue((('q0q1q3q4', 'b'), 'q0q1q3q4') in transitions)
        self.assertTrue((('q0q1q3q4', 'a'), 'q2q5') in transitions)
        self.assertTrue((('q2q5', 'a'), 'q0q1q3q4') in transitions)
        self.assertTrue((('q2q5', 'b'), 'q0q1q3q4') in transitions)
        self.assertEqual(len(transitions), 4)


if __name__ == '__main__':
    unittest.main()
