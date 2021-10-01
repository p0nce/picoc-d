module picocd.cstdlib.errno;

import core.stdc.errno;
import picocd.interpreter;
import picocd.variable;

__gshared EACCESValue = EACCES;
__gshared EADDRINUSEValue = EADDRINUSE;
__gshared EADDRNOTAVAILValue = EADDRNOTAVAIL;
__gshared EAFNOSUPPORTValue = EAFNOSUPPORT;
__gshared EAGAINValue = EAGAIN;
__gshared EALREADYValue = EALREADY;
__gshared EBADFValue = EBADF;
__gshared EBADMSGValue = EBADMSG;
__gshared EBUSYValue = EBUSY;
__gshared ECANCELEDValue = ECANCELED;
__gshared ECHILDValue = ECHILD;
__gshared ECONNABORTEDValue = ECONNABORTED;
__gshared ECONNREFUSEDValue = ECONNREFUSED;
__gshared ECONNRESETValue = ECONNRESET;
__gshared EDEADLKValue = EDEADLK;
__gshared EDESTADDRREQValue = EDESTADDRREQ;
__gshared EDOMValue = EDOM;
__gshared EEXISTValue = EEXIST;
__gshared EFAULTValue = EFAULT;
__gshared EFBIGValue = EFBIG;
__gshared EHOSTUNREACHValue = EHOSTUNREACH;
__gshared EIDRMValue = EIDRM;
__gshared EILSEQValue = EILSEQ;
__gshared EINPROGRESSValue = EINPROGRESS;
__gshared EINTRValue = EINTR;
__gshared EINVALValue = EINVAL;
__gshared EIOValue = EIO;
__gshared EISCONNValue = EISCONN;
__gshared EISDIRValue = EISDIR;
__gshared ELOOPValue = ELOOP;
__gshared EMFILEValue = EMFILE;
__gshared EMLINKValue = EMLINK;
__gshared EMSGSIZEValue = EMSGSIZE;
__gshared ENAMETOOLONGValue = ENAMETOOLONG;
__gshared ENETDOWNValue = ENETDOWN;
__gshared ENETRESETValue = ENETRESET;
__gshared ENETUNREACHValue = ENETUNREACH;
__gshared ENFILEValue = ENFILE;
__gshared ENOBUFSValue = ENOBUFS;
__gshared ENODATAValue = ENODATA;
__gshared ENODEVValue = ENODEV;
__gshared ENOENTValue = ENOENT;
__gshared ENOEXECValue = ENOEXEC;
__gshared ENOLCKValue = ENOLCK;
__gshared ENOLINKValue = ENOLINK;
__gshared ENOMEMValue = ENOMEM;
__gshared ENOMSGValue = ENOMSG;
__gshared ENOPROTOOPTValue = ENOPROTOOPT;
__gshared ENOSPCValue = ENOSPC;
__gshared ENOSRValue = ENOSR;
__gshared ENOSTRValue = ENOSTR;
__gshared ENOSYSValue = ENOSYS;
__gshared ENOTCONNValue = ENOTCONN;
__gshared ENOTDIRValue = ENOTDIR;
__gshared ENOTEMPTYValue = ENOTEMPTY;
__gshared ENOTRECOVERABLEValue = ENOTRECOVERABLE;
__gshared ENOTSOCKValue = ENOTSOCK;
__gshared ENOTSUPValue = ENOTSUP;
__gshared ENOTTYValue = ENOTTY;
__gshared ENXIOValue = ENXIO;
__gshared EOPNOTSUPPValue = EOPNOTSUPP;
__gshared EOVERFLOWValue = EOVERFLOW;
__gshared EOWNERDEADValue = EOWNERDEAD;
__gshared EPERMValue = EPERM;
__gshared EPIPEValue = EPIPE;
__gshared EPROTOValue = EPROTO;
__gshared EPROTONOSUPPORTValue = EPROTONOSUPPORT;
__gshared EPROTOTYPEValue = EPROTOTYPE;
__gshared ERANGEValue = ERANGE;
__gshared EROFSValue = EROFS;
__gshared ESPIPEValue = ESPIPE;
__gshared ESRCHValue = ESRCH;
__gshared ETIMEValue = ETIME;
__gshared ETIMEDOUTValue = ETIMEDOUT;
__gshared ETXTBSYValue = ETXTBSY;
__gshared EWOULDBLOCKValue = EWOULDBLOCK;
__gshared EXDEVValue = EXDEV;


/* creates various system-dependent definitions */
void StdErrnoSetupFunc(Picoc *pc)
{
    VariableDefinePlatformVar(pc, NULL, "EACCES", &pc.IntType, cast(AnyValue*)&EACCESValue, false);
    VariableDefinePlatformVar(pc, NULL, "EADDRINUSE", &pc.IntType, cast(AnyValue*)&EADDRINUSEValue, false);
    VariableDefinePlatformVar(pc, NULL, "EADDRNOTAVAIL", &pc.IntType, cast(AnyValue*)&EADDRNOTAVAILValue, false);
    VariableDefinePlatformVar(pc, NULL, "EAFNOSUPPORT", &pc.IntType, cast(AnyValue*)&EAFNOSUPPORTValue, false);
    VariableDefinePlatformVar(pc, NULL, "EAGAIN", &pc.IntType, cast(AnyValue*)&EAGAINValue, false);
    VariableDefinePlatformVar(pc, NULL, "EALREADY", &pc.IntType, cast(AnyValue*)&EALREADYValue, false);
    VariableDefinePlatformVar(pc, NULL, "EBADF", &pc.IntType, cast(AnyValue*)&EBADFValue, false);
    VariableDefinePlatformVar(pc, NULL, "EBADMSG", &pc.IntType, cast(AnyValue*)&EBADMSGValue, false);
    VariableDefinePlatformVar(pc, NULL, "EBUSY", &pc.IntType, cast(AnyValue*)&EBUSYValue, false);
    VariableDefinePlatformVar(pc, NULL, "ECANCELED", &pc.IntType, cast(AnyValue*)&ECANCELEDValue, false);
    VariableDefinePlatformVar(pc, NULL, "ECHILD", &pc.IntType, cast(AnyValue*)&ECHILDValue, false);
    VariableDefinePlatformVar(pc, NULL, "ECONNABORTED", &pc.IntType, cast(AnyValue*)&ECONNABORTEDValue, false);
    VariableDefinePlatformVar(pc, NULL, "ECONNREFUSED", &pc.IntType, cast(AnyValue*)&ECONNREFUSEDValue, false);
    VariableDefinePlatformVar(pc, NULL, "ECONNRESET", &pc.IntType, cast(AnyValue*)&ECONNRESETValue, false);
    VariableDefinePlatformVar(pc, NULL, "EDEADLK", &pc.IntType, cast(AnyValue*)&EDEADLKValue, false);
    VariableDefinePlatformVar(pc, NULL, "EDESTADDRREQ", &pc.IntType, cast(AnyValue*)&EDESTADDRREQValue, false);
    VariableDefinePlatformVar(pc, NULL, "EDOM", &pc.IntType, cast(AnyValue*)&EDOMValue, false);
    VariableDefinePlatformVar(pc, NULL, "EEXIST", &pc.IntType, cast(AnyValue*)&EEXISTValue, false);
    VariableDefinePlatformVar(pc, NULL, "EFAULT", &pc.IntType, cast(AnyValue*)&EFAULTValue, false);
    VariableDefinePlatformVar(pc, NULL, "EFBIG", &pc.IntType, cast(AnyValue*)&EFBIGValue, false);
    VariableDefinePlatformVar(pc, NULL, "EHOSTUNREACH", &pc.IntType, cast(AnyValue*)&EHOSTUNREACHValue, false);
    VariableDefinePlatformVar(pc, NULL, "EIDRM", &pc.IntType, cast(AnyValue*)&EIDRMValue, false);
    VariableDefinePlatformVar(pc, NULL, "EILSEQ", &pc.IntType, cast(AnyValue*)&EILSEQValue, false);
    VariableDefinePlatformVar(pc, NULL, "EINPROGRESS", &pc.IntType, cast(AnyValue*)&EINPROGRESSValue, false);
    VariableDefinePlatformVar(pc, NULL, "EINTR", &pc.IntType, cast(AnyValue*)&EINTRValue, false);
    VariableDefinePlatformVar(pc, NULL, "EINVAL", &pc.IntType, cast(AnyValue*)&EINVALValue, false);
    VariableDefinePlatformVar(pc, NULL, "EIO", &pc.IntType, cast(AnyValue*)&EIOValue, false);
    VariableDefinePlatformVar(pc, NULL, "EISCONN", &pc.IntType, cast(AnyValue*)&EISCONNValue, false);
    VariableDefinePlatformVar(pc, NULL, "EISDIR", &pc.IntType, cast(AnyValue*)&EISDIRValue, false);
    VariableDefinePlatformVar(pc, NULL, "ELOOP", &pc.IntType, cast(AnyValue*)&ELOOPValue, false);
    VariableDefinePlatformVar(pc, NULL, "EMFILE", &pc.IntType, cast(AnyValue*)&EMFILEValue, false);
    VariableDefinePlatformVar(pc, NULL, "EMLINK", &pc.IntType, cast(AnyValue*)&EMLINKValue, false);
    VariableDefinePlatformVar(pc, NULL, "EMSGSIZE", &pc.IntType, cast(AnyValue*)&EMSGSIZEValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENAMETOOLONG", &pc.IntType, cast(AnyValue*)&ENAMETOOLONGValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENETDOWN", &pc.IntType, cast(AnyValue*)&ENETDOWNValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENETRESET", &pc.IntType, cast(AnyValue*)&ENETRESETValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENETUNREACH", &pc.IntType, cast(AnyValue*)&ENETUNREACHValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENFILE", &pc.IntType, cast(AnyValue*)&ENFILEValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOBUFS", &pc.IntType, cast(AnyValue*)&ENOBUFSValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENODATA", &pc.IntType, cast(AnyValue*)&ENODATAValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENODEV", &pc.IntType, cast(AnyValue*)&ENODEVValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOENT", &pc.IntType, cast(AnyValue*)&ENOENTValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOEXEC", &pc.IntType, cast(AnyValue*)&ENOEXECValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOLCK", &pc.IntType, cast(AnyValue*)&ENOLCKValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOLINK", &pc.IntType, cast(AnyValue*)&ENOLINKValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOMEM", &pc.IntType, cast(AnyValue*)&ENOMEMValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOMSG", &pc.IntType, cast(AnyValue*)&ENOMSGValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOPROTOOPT", &pc.IntType, cast(AnyValue*)&ENOPROTOOPTValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOSPC", &pc.IntType, cast(AnyValue*)&ENOSPCValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOSR", &pc.IntType, cast(AnyValue*)&ENOSRValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOSTR", &pc.IntType, cast(AnyValue*)&ENOSTRValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOSYS", &pc.IntType, cast(AnyValue*)&ENOSYSValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTCONN", &pc.IntType, cast(AnyValue*)&ENOTCONNValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTDIR", &pc.IntType, cast(AnyValue*)&ENOTDIRValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTEMPTY", &pc.IntType, cast(AnyValue*)&ENOTEMPTYValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTRECOVERABLE", &pc.IntType, cast(AnyValue*)&ENOTRECOVERABLEValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTSOCK", &pc.IntType, cast(AnyValue*)&ENOTSOCKValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTSUP", &pc.IntType, cast(AnyValue*)&ENOTSUPValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENOTTY", &pc.IntType, cast(AnyValue*)&ENOTTYValue, false);
    VariableDefinePlatformVar(pc, NULL, "ENXIO", &pc.IntType, cast(AnyValue*)&ENXIOValue, false);
    VariableDefinePlatformVar(pc, NULL, "EOPNOTSUPP", &pc.IntType, cast(AnyValue*)&EOPNOTSUPPValue, false);
    VariableDefinePlatformVar(pc, NULL, "EOVERFLOW", &pc.IntType, cast(AnyValue*)&EOVERFLOWValue, false);
    VariableDefinePlatformVar(pc, NULL, "EOWNERDEAD", &pc.IntType, cast(AnyValue*)&EOWNERDEADValue, false);
    VariableDefinePlatformVar(pc, NULL, "EPERM", &pc.IntType, cast(AnyValue*)&EPERMValue, false);
    VariableDefinePlatformVar(pc, NULL, "EPIPE", &pc.IntType, cast(AnyValue*)&EPIPEValue, false);
    VariableDefinePlatformVar(pc, NULL, "EPROTO", &pc.IntType, cast(AnyValue*)&EPROTOValue, false);
    VariableDefinePlatformVar(pc, NULL, "EPROTONOSUPPORT", &pc.IntType, cast(AnyValue*)&EPROTONOSUPPORTValue, false);
    VariableDefinePlatformVar(pc, NULL, "EPROTOTYPE", &pc.IntType, cast(AnyValue*)&EPROTOTYPEValue, false);
    VariableDefinePlatformVar(pc, NULL, "ERANGE", &pc.IntType, cast(AnyValue*)&ERANGEValue, false);
    VariableDefinePlatformVar(pc, NULL, "EROFS", &pc.IntType, cast(AnyValue*)&EROFSValue, false);
    VariableDefinePlatformVar(pc, NULL, "ESPIPE", &pc.IntType, cast(AnyValue*)&ESPIPEValue, false);
    VariableDefinePlatformVar(pc, NULL, "ESRCH", &pc.IntType, cast(AnyValue*)&ESRCHValue, false);
    VariableDefinePlatformVar(pc, NULL, "ETIME", &pc.IntType, cast(AnyValue*)&ETIMEValue, false);
    VariableDefinePlatformVar(pc, NULL, "ETIMEDOUT", &pc.IntType, cast(AnyValue*)&ETIMEDOUTValue, false);
    VariableDefinePlatformVar(pc, NULL, "ETXTBSY", &pc.IntType, cast(AnyValue*)&ETXTBSYValue, false);
    VariableDefinePlatformVar(pc, NULL, "EWOULDBLOCK", &pc.IntType, cast(AnyValue*)&EWOULDBLOCKValue, false);
    VariableDefinePlatformVar(pc, NULL, "EXDEV", &pc.IntType, cast(AnyValue*)&EXDEVValue, false);
    VariableDefinePlatformVar(pc, NULL, "errno", &pc.IntType, cast(AnyValue*)&errno, true);    
}

