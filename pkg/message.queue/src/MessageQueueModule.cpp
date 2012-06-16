#include <Rcpp.h>

#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <vector>

#include "SharedCounter.hpp"

using namespace boost::interprocess;
using namespace std;
using namespace Rcpp;

#define DEFAULT_BUFFER_SIZE sizeof(char)*50000000
#define DEFAULT_QUEUE_SIZE 1000
typedef size_t size_type;
typedef vector<char> BufferType;

class RawMessageQueue
{
  public:
    RawMessageQueue() : _init(false), _pmq(NULL) {}
  
    ~RawMessageQueue() 
    {
      try
      {
        named_mutex mutex(open_or_create, (_name+"_counter_mutex").c_str());
        mutex.lock();
        bool destroyThis = (1==_counter.get()) ? true : false;
        if (_pmq) delete _pmq;
        if (_counter.get() == 1) destroyThis=true;
        mutex.unlock();
        if (destroyThis)
        {
          shared_memory_object::remove((string("sem.")+
            _name+"_counter_mutex").c_str());
          message_queue::remove(_name.c_str());
        }
      }
      catch(...)
      {
        Rprintf("Exception thrown in the RawMessageQueue destructor\n");
      }
    }
    
    void create(std::string name)
    {
      _buffer.resize(DEFAULT_BUFFER_SIZE);
      if (_init) return;
      _queueSize = DEFAULT_QUEUE_SIZE;
      _name = name;
      named_mutex mutex(open_or_create, (_name+"_counter_mutex").c_str());
      mutex.lock();
      _counter.init(_name + "_reference_counter");
      mutex.unlock();
      // May need to add some randomness to the mutex names.
      try
      {
        message_queue::remove(_name.c_str());
        _pmq = new message_queue(open_or_create, _name.c_str(), 
          DEFAULT_QUEUE_SIZE, DEFAULT_BUFFER_SIZE);
      }
      catch (interprocess_exception &e)
      {
        Rprintf("Interprocess exception caught:\n");
        Rprintf("%s\n", e.what());
      }
      catch (std::exception &e)
      {
        Rprintf("STL exception caught:\n");
        Rprintf("%s\n", e.what());
      }
      catch(...)
      {
        Rprintf("Unknown exception caught.\n");
      }
      _init = true;
    }
  
    void attach(std::string name)
    {
      if (_init) return;
      try
      {
        _name = name;
        _pmq = new message_queue(open_only, _name.c_str());
        _buffer.resize(_pmq->get_max_msg_size());
        named_mutex mutex(open_or_create, (_name+"_counter_mutex").c_str());
        mutex.lock();
        _counter.init(_name + "_reference_counter");
        mutex.unlock();
      }
      catch (interprocess_exception &e)
      {
        Rprintf("Interprocess exception caught:\n");
        Rprintf("%s\n", e.what());
      }
      catch (std::exception &e)
      {
        Rprintf("STL exception caught:\n");
        Rprintf("%s\n", e.what());
      }
      catch(...)
      {
        Rprintf("Unknown exception caught.\n");
      }
//      if (!_pmq) Rprintf("NULL pointer");
//      else Rprintf("Looks like we attached to a queue with buffer size %ld and queue size %ld.\n", 
//        _buffer.size(), _pmq->get_max_msg());
    }
  
    RawVector pop()
    {
      size_type recvSize;
      unsigned int priority;
//      Rprintf("It looks like there are %ld messages in the queue\n", _pmq->get_num_msg());
      try
      {
        _pmq->receive(&(_buffer[0]), _buffer.size(), recvSize, priority);
      }
      catch (interprocess_exception &e)
      {
        Rprintf("%s\n", e.what());
      }
//      Rprintf("Message size is %ld\n", _buffer.size());
      RawVector rv(recvSize);
      std::copy(_buffer.begin(), _buffer.begin()+recvSize, rv.begin());
      return rv;
    }

    void push(const RawVector &rv)
    {
      try
      {
        _pmq->send(&(rv[0]), rv.size(), 0);
      }
      catch (interprocess_exception &e)
      {
        Rprintf("%s\n", e.what());
      }
    }

// TODO: implement the buffer size function.

  protected:
    message_queue *_pmq;
    string _name;
    size_type _queueSize;
    bool _init;
    BufferType _buffer;
    SharedCounter _counter;
};

RCPP_MODULE(RawMessageQueue){
	using namespace Rcpp ;
  class_<RawMessageQueue>("RawMessageQueue")
    .constructor()
    .method( "create", &RawMessageQueue::create, "create the message queue")
    .method( "attach", &RawMessageQueue::attach, 
      "attach to an existing message queue")
    .method( "pop", &RawMessageQueue::pop, "pop from the message queue")
    .method( "push", &RawMessageQueue::push, "push onto the message queue")
  ;
}                     


