# https://stackoverflow.com/questions/4999233/how-to-raise-error-if-duplicates-keys-in-dictionary
class ComponentsDict(dict):
    def __init__(self, inp=None):
        if isinstance(inp,dict):
            super(ComponentsDict,self).__init__(inp)
        else:
            super(ComponentsDict,self).__init__()

    def __setitem__(self, k, v):
        try:
            self.__getitem__(k)
            raise ValueError("duplicate key '{0}' found".format(k))
        except KeyError:
            super(ComponentsDict,self).__setitem__(k,v)