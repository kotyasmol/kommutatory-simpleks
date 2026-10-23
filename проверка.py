"""Независимая проверка модели перебором вершин в точной арифметике.

Запуск: python3 проверка.py
Дополнительные библиотеки не нужны. Основное решение находится в решение.R.
"""
from csv import DictReader
from fractions import Fraction as Q
from itertools import combinations
from pathlib import Path
import json

ROOT = Path(__file__).resolve().parent


def read_table(name):
    with (ROOT / 'данные' / name).open(encoding='utf-8-sig', newline='') as file:
        return list(DictReader(file, delimiter=';'))


def solve(matrix, right):
    """Решить квадратную систему методом Гаусса с рациональными числами."""
    n = len(right)
    rows = [list(map(Q, row)) + [Q(b)] for row, b in zip(matrix, right)]
    for j in range(n):
        pivot = next((i for i in range(j, n) if rows[i][j]), None)
        if pivot is None:
            return None
        rows[j], rows[pivot] = rows[pivot], rows[j]
        div = rows[j][j]
        rows[j] = [x / div for x in rows[j]]
        for i in range(n):
            if i != j:
                factor = rows[i][j]
                rows[i] = [a - factor * b for a, b in zip(rows[i], rows[j])]
    return tuple(row[-1] for row in rows)


products = read_table('продукция.csv')
resources = read_table('ресурсы.csv')
names = [row['Модель'] for row in products]
A = [[Q(row[name]) for name in names] for row in resources]
b = [Q(row['Запас']) for row in resources]
c = [Q(row['Цена_руб']) - Q(row['Затраты_руб']) for row in products]
n = len(names)
assert n == 3, 'Проверка рассчитана на модель с тремя изделиями.'

# К ресурсным границам добавляются плоскости x_i = 0.
planes = A + [[Q(-int(i == j)) for j in range(n)] for i in range(n)]
bounds = b + [Q(0)] * n
vertices = set()
checked = feasible_candidates = 0
for indices in combinations(range(len(planes)), n):
    checked += 1
    x = solve([planes[i] for i in indices], [bounds[i] for i in indices])
    if x is not None and all(sum(a * v for a, v in zip(row, x)) <= rhs
                             for row, rhs in zip(planes, bounds)):
        feasible_candidates += 1
        vertices.add(x)
assert vertices, 'Допустимые вершины не найдены.'
objective = lambda x: sum(a * v for a, v in zip(c, x))
best = max(map(objective, vertices))
optimal = sorted(x for x in vertices if objective(x) == best)

# Сертификат ограниченности сверху и оптимальности для исходных данных.
y = list(map(Q, [0, 0, 1000, 2000, 1000, 0, 0, 0]))
assert len(y) == len(A)
assert all(sum(A[i][j] * y[i] for i in range(len(A))) >= c[j] for j in range(n))
dual = sum(a * v for a, v in zip(b, y))
assert best == dual, 'Двойственный вектор не подтверждает оптимум изменённой модели.'

with (ROOT / 'результаты/план.csv').open(encoding='utf-8-sig') as file:
    r_plan = list(DictReader(file, delimiter=';'))
assert [row['Модель'] for row in r_plan] == names
r_x = tuple(Q(row['Выпуск_шт'].replace(',', '.')) for row in r_plan)
assert all(abs(a - v) <= Q(1, 10**7) for a, v in zip(r_x, optimal[0]))
assert abs(objective(r_x) - best) <= Q(1, 10**5)

result = {
    'Метод': 'Перебор пересечений ограничений; точная рациональная арифметика',
    'Проверено_комбинаций': checked,
    'Допустимых_кандидатов': feasible_candidates,
    'Уникальных_допустимых_вершин': len(vertices),
    'Оптимальных_вершин': len(optimal),
    'Оптимальный_план': dict(zip(names, map(str, optimal[0]))),
    'Маржинальная_прибыль_руб': str(best),
    'Двойственная_оценка_руб': str(dual),
    'Совпадение_с_решением_R': True,
}
output = ROOT / 'результаты/проверка.json'
output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
print('Проверка завершена. Оптимальный план:', result['Оптимальный_план'])
print('Маржинальная прибыль:', best, 'руб.')
