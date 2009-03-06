
env = Environment(tools=['default'])
env.AppendUnique(CCFLAGS = ['-O3'])
Export('env')

env.installLocalLib = '#/lib'
env.installLocalInc = '#/include'
env.installLocalBin = '#/bin'

SConscript('Libraries/SConscript')
SConscript('Applications/SConscript')
