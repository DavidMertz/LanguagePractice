from collections import *
import array
import queue
import asyncio
import multiprocessing
import warnings
import numpy as np
import pandas as pd
import xarray as xr
# https://pypi.org/project/python-linkedlist/#description
import linkedadt

warnings.filterwarnings('ignore')
aggregates = """
''
list()
tuple()
dict()
set()
bytearray()
bytearray(1)
bytearray([0])
array.array('i')
array.array('i', [])
Nothing = namedtuple("EmptyNamedTuple", []); Nothing()
deque()
deque([])
ChainMap()
queue.Queue()
asyncio.Queue()
multiprocessing.Queue()
np.ndarray(1,)
np.ndarray((1,0))
np.empty((1,))
np.zeros((1,))
np.zeros((2,))
np.ones((1,))
np.ones((2,))
pd.Series()
pd.DataFrame()
xr.DataArray()
linkedadt.LinkedList()
""".strip().split('\n')

for aggregate in aggregates:
    if ';' in aggregate:
        setup, aggregate = aggregate.split(';')
        exec(setup.strip())
        aggregate = aggregate.strip()
    val = eval(aggregate)
    try:
        truth = bool(val)
    except:
        truth = "No Truthiness"
    try:
        length = len(val)
    except:
        length = "No length"

    val = repr(val).split('\n')[0].strip()
    print(f"Expr: {aggregate} | Value: {val}")
    print(f"  Truth: {truth} | Length: {length}")
