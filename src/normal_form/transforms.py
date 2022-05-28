from pprint import pprint
from copy import deepcopy
import re
from src.mapper.TreeMapper import ContextAnd, ContextOr, Rule, Epsilon
from src.report.tools import Report



def concat_dicts(dict1, dict2):
    res_dict = dict(dict1)
    keys2 = dict2.keys()
    for key in keys2:
        if key in res_dict.keys():
            if res_dict[key].type() != 'OR':
                if dict2[key].type() != 'OR':
                    new_rule = ContextOr()
                    new_rule.add(res_dict[key])
                    new_rule.add(dict2[key])
                    res_dict[key] = new_rule
                else:
                    res_dict[key] = dict2[key].add(res_dict[key])
            else:
                res_dict[key] = res_dict[key].add(dict2[key])
        else:
            res_dict[key] = dict2[key]
    return res_dict

#---------------------------------------------

def isTerminal(el, nonterms):
    """Функция проверки порождаемости правила 
    el - правило
    nonterms -- какие из нетерминалов правила являются порождающими
    """
    if el.type() == 'EPSILON':
        return el.isTerminal()

    if el.type() == 'RULE':
        if el.isTerminal():
            return True
        if el.val() in nonterms:
            return True
        return False

    if el.type() == 'AND':
        for item in el.getItems():
            if not isTerminal(item, nonterms):
                return False
        return True
    if el.type() == 'OR':
        for item in el.getItems():
            if isTerminal(item, nonterms):
                return True
        return False


def delete_item_consist(el, trash):
    """Функция удаления правил с непорождающими нетерминалами
    el -- правило
    trash -- массив непорождающих нетерминалов
    """
    if el.type() == 'EPSILON':
        return el
    if el.type() == 'RULE':
        if el.isTerminal() or el.val() not in trash:
            return el
        return None
    if el.type() == 'AND':
        for item in el.getItems():
            if delete_item_consist(item, trash) is None:
                return None
        return el
    if el.type() == 'OR':
        remove_idx = []
        for i in range(len(el)):
            rul = delete_item_consist(el[i], trash)
            if rul is None:
                remove_idx.append(i)
        for i in reversed(range(len(remove_idx))):
            el.removeIdx(remove_idx[i])
        if len(el) == 0:
            return None
        return el


def delete_nonterms(start_dict, nonterms):
    res_dict = dict(start_dict)
    while (True):
        # удаляем все непорождающие нетерминалы и правила с ними
        step_trash = []
        step_dict = dict(res_dict)
        for t, rules in step_dict.items():
            if t in nonterms:
                res_dict.pop(t)
                continue
            new_rules = delete_item_consist(rules, nonterms)
            if new_rules is None:
                res_dict.pop(t)
                step_trash.append(t)
            else:
                res_dict[t] = new_rules
        if len(step_trash) == 0:
            break
        trash = step_trash
    return res_dict


def delete_nongenerating(start_dict):
    """Функция удаления всех непорождающих терминалов
    start_dict - словарь описывающий все правила языка"""
    help_dict = dict(start_dict)
    gen_nonterm = set()

    while True:
        # находим все порождающие терминалы
        step_nonterm = set(gen_nonterm)
        step_dict = dict(help_dict)
        for t, rules in step_dict.items():
            if isTerminal(rules, gen_nonterm):
                step_nonterm.add(t)
                help_dict.pop(t)
        if gen_nonterm == step_nonterm:
            break
        gen_nonterm = set(step_nonterm)

    trash = list(help_dict.keys())  # непорождающие терминалы

    res_dict = delete_nonterms(start_dict, trash)
    return res_dict


def get_all_nonterminals(rule):
    """Функция поиска всех нетерминалов в правиле rule"""
    res_set = set()
    if rule.type() == 'RULE' and not rule.isTerminal():
        res_set.add(rule.val())
    if rule.type() == 'AND' or rule.type() == 'OR':
        for item in rule.getItems():
            res_set = res_set.union(get_all_nonterminals(item))
    return res_set


def delete_unreachable(start_dict, start_name):
    """Функция удаления всех вершин недостижымых из стартовой"""
    reachable = {start_name, }
    inner_reachable = set(reachable)
    while len(inner_reachable) > 0:
        # находим все достижымые вершины
        step_reachable = set()
        for nont in inner_reachable:
            step_reachable = step_reachable.union(get_all_nonterminals(start_dict[nont]))
        inner_reachable = step_reachable.difference(reachable)
        reachable = reachable.union(step_reachable)
    non_reachable = set(start_dict.keys()).difference(reachable)
    res_dict = delete_nonterms(start_dict, non_reachable)  # удаляем недостижымые
    return res_dict


def delete_useless_non_terminal(start_dict, start_name):
    """Функция удаления бесполезных нетерминалов(непорождающих и недостижымых)
    Оба шага выполняются неоптимизированно за O(n^2), можно улучшить
    """
    res_dict = delete_nongenerating(start_dict)
    res_dict = delete_unreachable(res_dict, start_name)
    return res_dict


# -----------------------------------

def make_new_rules(name, rule, start_rule_num):
    if rule.type() in ['EPSILON', 'RULE']:
        return {name: rule}, start_rule_num

    if rule.type() == 'AND':
        if len(rule) < 3:
            return {name: rule}, start_rule_num
        else:
            new_rule_name = f"_{start_rule_num}"
            new_rule = Rule(new_rule_name, False)
            start_rule_num += 1
            items = rule.getItems()
            new_and = ContextAnd()
            new_and.add(items[0])
            new_and.add(new_rule)
            res_dict = {name: new_and}
            rule.removeIdx(0)
            new_dict, start_rule_num = make_new_rules(new_rule_name, rule, start_rule_num)
            res_dict = concat_dicts(res_dict, new_dict)
            return res_dict, start_rule_num

    if rule.type() == 'OR':
        res_dict = {}
        items = rule.getItems()
        for item in items:
            new_dict, start_rule_num = make_new_rules(name, item, start_rule_num)
            res_dict = concat_dicts(res_dict, new_dict)
        return res_dict, start_rule_num


def delete_long_right_part(start_dict):
    res_dict = dict(start_dict)
    rule_num = 0
    for key, rules in start_dict.items():
        new_dict, rule_num = make_new_rules(key, rules, rule_num)
        if len(new_dict) != 1:
            res_dict.pop(key)
            res_dict = concat_dicts(res_dict, new_dict)
    return res_dict

#--------------------------------------------------------

def isEpsConsist(rules, nonterms):
    if rules.type()=='EPSILON':
        return True
    if rules.type() == 'RULE' and not rules.isTerminal():
        return (rules.val() in nonterms)
    if rules.type() == 'AND':
        for item in rules.getItems():
            if not isEpsConsist(item, nonterms):
                return False
        return True
    if rules.type() == 'OR':
        items = rules.getItems()
        for item in items:
            if isEpsConsist(item, nonterms):
                return True
        return False

def find_epsilon_rules(start_dict):
    res = []
    help_dict = dict(start_dict)
    while True:
        # находим все порождающие терминалы
        step_res = []
        step_dict = dict(help_dict)
        for key, rules in step_dict.items():
            isEps = isEpsConsist(rules, res)
            if isEps:
                step_res.append(key)
                help_dict.pop(key)
        if len(step_res) == 0:
            break
        res += step_res
    return res


def deleteNonTerminal(rules, nonterm):
    if rules.type() in ['EPSILON', 'RULE']:
        return rules
    if rules.type() == 'AND':
        if len(rules) == 1:
            return rules 
        items = rules.getItems()
        if items[0].val() == nonterm:
            new_rules = ContextOr()
            new_rules.add(rules)
            new_rules.add(items[1])
            return new_rules
        if items[1].val() == nonterm:
            new_rules = ContextOr()
            new_rules.add(rules)
            new_rules.add(items[0])
            return new_rules
        return rules
    if rules.type() == 'OR':
        items = rules.getItems()
        new_rules = ContextOr()
        for item in items:
            new_items= deleteNonTerminal(item, nonterm)
            new_rules.add(new_items)
        return new_rules

def delete_eps_rule(rules):
    if rules.type() == 'EPSILON':
        return None
    if rules.type() in ['RULE', 'AND']:
        return rules
    if rules.type() =='OR':
        clear_rules = ContextOr()
        for item in rules.getItems():
            if item.type() != 'EPSILON':
                clear_rules.add(item)
        if len(clear_rules) != 0:
            return clear_rules
        return None

def delete_eps_rules(start_dict):
    res_dict = dict()
    for k, r in start_dict.items():
        clear_r = delete_eps_rule(r)
        if clear_r is not None:
            res_dict[k] = clear_r
    return res_dict


def delete_epsilons(start_dict, start_name):
    res_dict = dict(start_dict)
    eps_consist = find_epsilon_rules(start_dict)
    for name in eps_consist:
        step_dict = dict(res_dict)
        for k, r in step_dict.items():
            res_dict[k] = deleteNonTerminal(r, name)
    res_dict = delete_eps_rules(res_dict)
    start_rule_name = "_START"
    # TODO: поменять
    new_rule = Rule(start_name, False)
    # TODO: поменять на маркер стартовой вершины
    if start_name in eps_consist:
        start_rule = ContextOr()
        new_eps = Epsilon()
        start_rule.add(new_eps)
        start_rule.add(new_rule)
        res_dict[start_rule_name] = start_rule
    else:
        res_dict[start_rule_name] = new_rule
    return res_dict

#-----------------------------------------------------------------
def delete_self_made_rules(start_dict):
    res_dict = dict()
    for key, rules in start_dict.items():
        if rules.type() in ['EPSILON', 'AND']:
            res_dict[key] = rules
        elif rules.type() == 'RULE':
            if not rules.isTerminal() and rules.val() == key:
                continue
            else:
                res_dict[key] = rules
        elif rules.type() == 'OR':
            items = rules.getItems()
            for item in items:
                if item.type() == 'RULE' and item.val() == key:
                    rules.removeVal(item)
            if len(rules)!=0:
                res_dict[key] = rules
    return res_dict

def replace_chains_in_rule(rule, start_dict):
    if rule.type() in ['EPSILON', 'AND']:
        return rule, False
    if rule.type() == 'RULE':
        if rule.isTerminal():
            return rule, False
        else:
            rule_val = rule.val()
            return start_dict[rule_val], True
    if rule.type() == 'OR':
        new_rules = ContextOr()
        items = rule.getItems()
        isChanged = False
        for item in items:
            n_r, isC = replace_chains_in_rule(item, start_dict)
            new_rules.add(n_r)
            if isC:
                isChanged = True
        return new_rules, isChanged
        

def replace_all_chains(start_dict):
    res_dict = dict()
    help_dict = dict(start_dict)
    notStop = True
    while notStop:
        start_dict = delete_self_made_rules(start_dict)
        inner_dict = dict(start_dict)
        notStop = False
        for key, rules in inner_dict.items():
            new_rule, isCh = replace_chains_in_rule(rules, help_dict)
            notStop = notStop or isCh
            if not isCh: 
                res_dict[key] = new_rule
                help_dict[key] = new_rule
                start_dict.pop(key)
                continue
            start_dict[key] = new_rule
            help_dict[key] = new_rule
    return res_dict

def delete_chain_products(start_dict):
    res_dict = replace_all_chains(start_dict)
    return res_dict


#-------------------------------------------------------------
def find_terminals_in_long_rules(rules):
    res = set()
    if rules.type() != 'OR' and len(rules) == 1:
        return res
    if rules.type() == 'AND': # and len(rules==2)
        items = rules.getItems()
        if items[0].isTerminal():
            res.add(items[0].val())
        if items[1].isTerminal():
            res.add(items[1].val())
        return res
    if rules.type() == 'OR':
        items = rules.getItems()
        for item in items:
            res = res | find_terminals_in_long_rules(item)
        return res

def find_terminals_in_long_right_part(start_dict):
    res = set()
    for k, r in start_dict.items():
        res = res | find_terminals_in_long_rules(r)
    return res

def find_exists_terminal_rules(start_dict):
    res = dict()
    for k, rule in start_dict.items():
        if rule.type() == 'RULE' and rule.isTerminal():
            res[rule.val()] = k
    return res

def update_rule(rule, dict_of_nonterms):
    if rule.type() != 'OR' and len(rule) == 1:
        return rule
    if rule.type() == 'AND': # and len(rule) == 2
        items = rule.getItems()
        new_and = ContextAnd()
        if items[0].isTerminal():
            rule1 = Rule(dict_of_nonterms[items[0].val()], False)
        else:
            rule1 = items[0]
        if items[1].isTerminal():
            rule2 = Rule(dict_of_nonterms[items[1].val()], False)
        else:
            rule2 = items[1]
        new_and.add(rule1)
        new_and.add(rule2)
        return new_and
    if rule.type() == 'OR':
        new_or = ContextOr()
        items = rule.getItems()
        for item in items:
            new_item = update_rule(item, dict_of_nonterms)
            new_or.add(new_item)
        return new_or

    
def update_with_rules(start_dict, dict_of_nonterms):
    res_dict = dict()
    for k, r in start_dict.items():
        res_dict[k] = update_rule(r, dict_of_nonterms)
    return res_dict

def delete_right_terminals(start_dict):
    res_dict = dict(start_dict)
    all_right_terms = find_terminals_in_long_right_part(res_dict)
    dict_of_names = find_exists_terminal_rules(res_dict)
    new_names = dict()
    for term_name in all_right_terms:
        if term_name not in dict_of_names.keys():
            new_name = f"__{term_name}"
            dict_of_names[term_name] = new_name
            new_names[term_name] = new_name

    res_dict = update_with_rules(res_dict, dict_of_names)

    for val, name in new_names.items():
        new_rule = Rule(val, True)
        res_dict[name] = new_rule
    
    return res_dict


def transform(start_dict, start_name, report):    
    name_of_res_file = f'cnf_steps_{start_name}.txt'

    cursor = report.section('transform')
    with open(name_of_res_file, 'w') as res_file:
        res_file.write('start rules:\n')
        pprint(start_dict, stream=res_file)
        res_dict = delete_useless_non_terminal(start_dict, start_name)
        res_file.write('\nremove all unreachable and nongenerating rules:\n')
        
        cursor.part('one').then('info').set_val('remove all unreachable and nongenerating rules')
        cursor.part('one').then('dict').set_dict(res_dict)

        pprint(res_dict, stream=res_file)
        res_dict = delete_long_right_part(res_dict)
        res_file.write('\n replace all long rules:\n')

        cursor.part('two').then('info').set_val('replace all long rules')
        cursor.part('two').then('dict').set_dict(res_dict)

        pprint(res_dict, stream=res_file)
        res_dict = delete_epsilons(res_dict, start_name)
        res_file.write('\ndelete epsilon rules:\n')
 
        cursor.part('three').then('info').set_val('delete epsilon rules')
        cursor.part('three').then('dict').set_dict(res_dict)

        pprint(res_dict, stream=res_file)
        res_dict = delete_chain_products(res_dict)
        res_dict = delete_useless_non_terminal(res_dict, '_START')
        res_file.write('\nremove all chain products:\n')

        cursor.part('four').then('info').set_val('remove all chain products')
        cursor.part('four').then('dict').set_dict(res_dict)

        pprint(res_dict, stream=res_file)
        res_dict = delete_right_terminals(res_dict)
        res_file.write('\ndelete terminals in the long right parts:\n')
        
        cursor.part('five').then('info').set_val('delete terminals in the long right parts')
        cursor.part('five').then('dict').set_dict(res_dict)

        pprint(res_dict, stream=res_file)
        # нужно ли еще раз почистить от недостижимых вершин?
        #res_dict = delete_useless_non_terminal(res_dict, '_START')

    return res_dict, name_of_res_file
