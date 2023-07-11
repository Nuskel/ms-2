proto List<T>:

  member data: [T];
  
  def add(e: T) forward data.append

end

def List<T>::map(fn: lambda(T, N)): List<N>
  let tmp: List<N> = []

  for e in data do
    tmp.add(fn(e))
  end

  return tmp
end