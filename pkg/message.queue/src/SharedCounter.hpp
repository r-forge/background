#ifndef _SHARED_COUNTER_H
#define _SHARED_COUNTER_H

#include <string>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/shared_memory_object.hpp>

//#include <iostream>
//using namespace std;

// Note: Shared Counters are not mutex protected.
class SharedCounter
{
  public:

    SharedCounter(): _pVal(NULL),_pRegion(NULL){};
    ~SharedCounter(){reset();};

    bool init( const std::string &resourceName )
    {
      _resourceName = resourceName;
      // See if we are connecting for the first time.
      try
      {  
//        cout << "creating a new counter " << resourceName << endl;
        boost::interprocess::shared_memory_object shm(
          boost::interprocess::create_only,
          _resourceName.c_str(), 
          boost::interprocess::read_write);
        // It's a new counter.
        shm.truncate( sizeof(std::size_t) );
        _pRegion = new boost::interprocess::mapped_region(shm, 
          boost::interprocess::read_write);
        _pVal = reinterpret_cast<std::size_t*>(_pRegion->get_address());
        *_pVal = 1;
      }
      catch(std::exception &ex)
      {
//        cout << "connecting to an existing counter" << resourceName << endl;
        // We are connecting to an existing counter.
        boost::interprocess::shared_memory_object shm(
          boost::interprocess::open_only,
          _resourceName.c_str(), 
          boost::interprocess::read_write);
        _pRegion = new boost::interprocess::mapped_region(shm, 
          boost::interprocess::read_write);
        _pVal = reinterpret_cast<std::size_t*>(_pRegion->get_address());
        ++(*_pVal);
      }
      return true;
    }

  public:
    std::size_t get() const
    {
      return _pVal == NULL ? 0 : *_pVal;
    }
    
    bool reset()
    {
      if (_pVal)
      {
        --(*_pVal);
        if (get() == 0)
        {
          boost::interprocess::shared_memory_object::remove(_resourceName.c_str());
          _resourceName="";
        }
        delete _pRegion;
      }
      _pVal = NULL;
      _resourceName = "";
      return true;
    }
  
  protected:
    std::size_t *_pVal;  
    boost::interprocess::mapped_region *_pRegion;
    std::string _resourceName;
};
#endif //_SHARED_COUNTER_H
