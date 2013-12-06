#ifndef SS__DRC_INTERFACE_H
#define SS__DRC_INTERFACE_H

namespace ss{

class DRCInterface
{
public:
    DRCInterface() {}
    virtual ~DRCInterface() {}


private:
    DRCInterface( const DRCInterface& source );
    void operator = ( const DRCInterface& source );
};

}
#endif // __DRC_INTERFACE_H
