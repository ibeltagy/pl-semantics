#ifndef __DRC_INTERFACE_H
#define __DRC_INTERFACE_H

class DRCInterface
{
public:
    DRCInterface() {}
    virtual ~DRCInterface() {}


private:
    DRCInterface( const DRCInterface& source );
    void operator = ( const DRCInterface& source );
};


#endif // __DRC_INTERFACE_H
