import numpy as np

import pdb

def text_to_vec(x):
  # pdb.set_trace()
  # http://bwlewis.github.io/1000_genomes_examples/PCA.html
  return np.array([float(r) for r in x.strip().replace(" ", "").split(",")])
  
def FileIter(file_name, chunk_size = 10, skip = 1):
  with open(file_name) as f:
    if skip > 0:
      [f.readline() for s in range(skip)]
    eof = False
    while not eof:
      lines = [f.readline() for s in range(chunk_size)]
      ret = np.array([text_to_vec(x) for x in lines if len(x) > 0])
      current_pos = f.tell()
      if f.readline() == '':
        eof = True
      f.seek(current_pos)
      yield ret

next(FileIter("x.csv"))

i = 0
for y_chunk, x_chunk in zip(FileIter("y.csv"), FileIter("x.csv")):
  print("Chunk ", i)
  print("Y Chunk")
  print(y_chunk)
  print("x Chunk")
  print(x_chunk)
  i += 1
