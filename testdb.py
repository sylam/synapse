from synapse.GE_DBInstance import Context, move, setup
from synapse.GE_DataFunctions import InstanceFunctionCatalog, ModelFunctionCatalog

if __name__ == '__main__':
    from synapse.GE_Environment import Environment

    if 'env' not in locals():
        data = {}
        env = Environment()
        Functions = ModelFunctionCatalog(env)
        InstanceFns = InstanceFunctionCatalog(env)
        cx = Context(env, Functions, InstanceFns)

    # cProfile.run("cx.LoadSystem( 'sqlite:////MarketRisk.db' )", 'loader.prof')

    # cx.LoadSystem( 'sqlite:////MarketRisk.db' )
    # cx.InstanceRoot.sub_instances[2].child.Execute([])
    # setup('sqlite:///arena.db')
    cx.LoadSystem('sqlite:///arena.db')
    cx.InstanceRoot.Execute([])
    cx.Save()
