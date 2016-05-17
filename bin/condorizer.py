#!/lusr/bin/python

"""
A very simple wrapper for a function call, making a corresponding condor script
and submitting it.
"""

import os, sys

def condorize (args):
    CondorScript = """
    universe = vanilla

    Executable = %s
    Requirements = InMastodon && (machine =!= "nasil-8.cs.utexas.edu")
    %s

    +Group   = "GRAD"
    +Project = "AI_ROBOTICS"
    +ProjectDescription = "Semantic Textual Similarity using Markov Logic Network"

    Arguments = %s
    Queue 
    """

    OutputLine = """
    Error = %s.err
    Output = %s.out
    """
    RawExecutable = args[1]
    Arguments = ' '.join(args[2:-1])
    OutputFile = args[-1]

    Executable = os.popen('/bin/which %s' % RawExecutable).read()
    CurrentDir = os.popen('/bin/pwd').read()

    # remove path information
    SafeOutputFile = '-'.join(OutputFile.split('/'))

    if OutputFile == "/dev/null":
      outputlines = ""
    else:
      outputlines = OutputLine % (OutputFile, OutputFile)

    condor_file = '/tmp/%s.condor' % (SafeOutputFile)
    f = open(condor_file, 'w')
    f.write(CondorScript % ( RawExecutable, outputlines, Arguments))
    f.close()   

    #print args
    print condor_file
    os.popen('/lusr/opt/condor/bin/condor_submit %s' % condor_file)

if __name__ == '__main__':
    condorize(sys.argv)
