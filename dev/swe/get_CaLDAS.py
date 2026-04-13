# Created on Mon Jan 30 2016
# @author Jan Talsma
# Modified by Dave Casson, Aug 2018
# Modified by Ben Balk, Oct 2023

#Script developed for RDPA precipitation GRIB2 download

import os
import time
from datetime import timedelta, datetime
import fews_adapt_py3 as fa
import optparse as op
import urllib3
import threading
import certifi

global numberFilesDownloaded
global FilesFailed

# Don't remove this line.  The idna encoding
# is used by getaddrinfo when dealing with unicode hostnames,
# and in some cases, there appears to be a race condition
# where threads will get a LookupError on getaddrinfo() saying
# that the encoding doesn't exist.  Using the idna encoding before
# running any CLI code (and any threads it may create) ensures that
# the encodings.idna is imported and registered in the codecs registry,
# which will stop the LookupErrors from happening.
# See: https://bugs.python.org/issue29288
u''.encode('idna')

def getRunInfo(settingsDict, runInfoFile,logger):
    ''' Funcion to retrieve run time information from FEWS '''

    logger.info('Loading run time information from run info file')

    workDir   = fa.getElementFromRuninfo(runInfoFile, 'workDir')
    startTime = fa.getStartTimefromRuninfo(runInfoFile)
    endTime   = fa.getEndTimefromRuninfo(runInfoFile)


    # add to settings dict, currently not used
    settingsDict['startTime'] = startTime
    settingsDict['endTime']   = endTime
    settingsDict['workDir']   = workDir


    settingsDict['diagnosticFile']  = fa.getElementsFromRuninfo(runInfoFile, 'outputDiagnosticFile')
    settingsDict['destinationDir'] =  fa.getKeyValueFromRunInfo(runInfoFile, 'destinationDir', True)

    return settingsDict

def start_thread(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads):
    ''' Function implement threading for data download'''
    global numFilesDownloaded
    global FilesFailed
    threadLimiter.acquire()

    thread = threading.Thread(target=data_download, args=(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads))
    thread.start()
    return thread

def data_download(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads):
    ''' Function to download data from url, called by threading'''
    attempts = 0
    outputFile = os.path.join(outputDir,filename)

    if os.path.isfile(outputFile):
        logger.info('File %s already exists. Download cancelled' %filename)
        if useThreading:
            threadLimiter.release()
    else:
        try:
            http = urllib3.PoolManager(timeout=10, retries=3)
            response = http.request('GET', url, preload_content=False)
            with open(outputFile, 'wb') as out:
                while True:
                    data = response.read(1024)
                    if not data:
                        break
                    out.write(data)
            response.release_conn()
            url = url.replace('&', '&amp;')
            logger.info('Downloading url complete [%s]' % url)
            global NumFilesDownloaded
            NumFilesDownloaded += 1
            
            #14-4-2012-AvL: check file size
            if os.path.getsize(outputFile) < 100 * 1024: 
                os.remove(outputFile)
                logger.info('file %s deleted because of incorrect filesize' %filename)
                
        except urllib3.exceptions.NewConnectionError:
            logger.error('Downloading url failed [%s]' % url)
            global FilesFailed
            FilesFailed.append(filename)
    if useThreading:
        #Log Current number of threads
        logger.info('Maximum Number Concurrent of Threads Allowed = %d, Number of Threads Active = %d' %(maximumNumberOfThreads, threading.active_count()-1))
        threadLimiter.release()

    return True

def mainscript():

    start = time.time()
    txtLogFile   = './get_CaLDAS.log'

    # Set the default reference time in case one isn't specified (set for now) - needed to be consistent with %H%M formating
    dt  = default=datetime.utcnow()
    #dt  = dt.replace(hour=0, minute=0, second=0, microsecond=0) # Returns a copy
    dt  = dt.strftime("%Y%m%d%H%M")

    logger = fa.setlogger(txtLogFile,'log')
    logger.info('Started CaLDAS download script at %s', str(datetime.fromtimestamp(start)))
    cmdLineArgs = op.OptionParser()
    cmdLineArgs.add_option('--runInfoFile',  '-r',  default='none')
    cmdLineArgs.add_option('--num_days','-s',  default='14')
    cmdLineArgs.add_option('--delay','-d',  default='12')
    cmdLineArgs.add_option('--threading',    '-t',  default='1')
    cmdLineArgs.add_option('--maxThreads',    '-n',  default='10')
    cmdLineArgs.add_option('--referenceTime','-z',  default=dt)


    cmdOptions, cmdArguments = cmdLineArgs.parse_args()
    runInfoFile     = cmdOptions.runInfoFile
    num_days     = int(cmdOptions.num_days)
    delayHours      = int(cmdOptions.delay)
    threadingOpt    = int(cmdOptions.threading)
    maximumNumberOfThreads    = int(cmdOptions.maxThreads)
    referenceTime   = datetime.strptime(cmdOptions.referenceTime, "%Y%m%d%H%M")
    # interpret option whether to use threading
    useThreading=False
    threadLimiter = threading.BoundedSemaphore(maximumNumberOfThreads) #Additional Variable to Control Threading, defined even if threading not used

    if threadingOpt==1:
        useThreading=True
        logger.info('Threading enabled with maximum number of threads =  %d'  %maximumNumberOfThreads)


    settingsDict = dict()

    if 'none' in runInfoFile:
        print('Output Dir may not be correctly set')
        #if not command line is provided, download regional model
        settingsDict['destinationDir'] = os.path.join( os.path.dirname( __file__ ), '..//..//..//Import//CaLDAS//')
        # delay compared to utc+00 for downloading the latest forecast (required time for simulation etc.)
    else:
        # in this case a run file is provided as well as other command line arguments
        settingsDict = getRunInfo(settingsDict, runInfoFile,logger)

    xmlLog = False
    xmlLogFile = 'get_CaLDAS_log.xml'

    #if settingsDict.has_key('diagnosticFile'):
    if 'diagnosticFile' in settingsDict:
        xmlLog = True

    outputDir = settingsDict['destinationDir']
    delaySeconds = delayHours*3600
    logger.info(xmlLogFile)

    if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file

    # reference time is utc+00 - configurable delay (cfs data is in utc+00)
    refDate = referenceTime-timedelta(days=num_days)-timedelta(seconds=delaySeconds)
    # time stamps
    hStr_list = ['00','06','12', '18']

    # list with parameters to download
    #parameters = [    '_Sfc_'    ]# for rotated pole
    parameters = [    '_SnowWaterEquiv_','_SnowDepth_'    ]

    urlBase = 'https://dd.alpha.weather.gc.ca/model_nsrps-caldas/2.5km/'
    threads = []
    global NumFilesDownloaded
    global FilesFailed

    NumFilesDownloaded = 0
    FilesFailed = []

    for day in range(0,num_days+1):

        dStr18 = refDate.date().strftime("%Y%m%d")
        dStr12 = refDate.date().strftime("%Y%m%d")

        refDate = refDate+timedelta(days=1)
        dStr = refDate.date().strftime("%Y%m%d")
        
        for hStr in hStr_list:
            for parameter in parameters:

                if hStr == '18':
                    #filename = dStr18 + 'T' + hStr + 'Z_MSC_RDPA_APCP-Accum6h' + parameter + 'RLatLon0.09_PT0H.grib2'    #for rotated pole
                    #url = urlBase + dStr + '/WXO-DD/model_rdpa/10km/' + hStr + '/' + filename                            #for rotated pole
                    #filename = dStr18 + 'T' + hStr + 'Z_MSC_NSRPS-CaLDAS_SnowWaterEquiv' + parameter + 'RLatLon0.0225_PT0H.nc'
                    filename = dStr18 + 'T' + hStr + 'Z_MSC_NSRPS-CaLDAS' + parameter + 'Sfc_RLatLon0.0225_PT0H.nc'
                    url = urlBase + hStr + '/' + filename
                elif hStr == '12':
                    #filename = dStr12 + 'T' + hStr + 'Z_MSC_NSRPS-CaLDAS_SnowWaterEquiv' + parameter + 'RLatLon0.0225_PT0H.nc'
                    filename = dStr12 + 'T' + hStr + 'Z_MSC_NSRPS-CaLDAS' + parameter + 'Sfc_RLatLon0.0225_PT0H.nc'
                    url = urlBase + hStr + '/' + filename
                else:
                    #filename = dStr + 'T' + hStr + 'Z_MSC_NSRPS-CaLDAS_SnowWaterEquiv' + parameter + 'RLatLon0.0225_PT0H.nc'
                    filename = dStr + 'T' + hStr + 'Z_MSC_NSRPS-CaLDAS' + parameter + 'Sfc_RLatLon0.0225_PT0H.nc'
                    url = urlBase + hStr + '/' + filename

                if useThreading:
                    threads.append(start_thread(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads))
                else:
                    data_download(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads)


    if useThreading: [thread.join() for thread in threads]

    logger.info('Completed downloading %d files in %d seconds'%(NumFilesDownloaded, time.time() - start))

    for failedfile in FilesFailed:
        logger.error('File failed to download: %s' %failedfile)

    if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file

if __name__ == '__main__':
    ### this is not executed when script is imported from another script
    mainscript()
