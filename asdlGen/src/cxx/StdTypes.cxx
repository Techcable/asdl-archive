/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_include asdl_base.hxx
  --line_width 74
  --no_action false
  --output_directory ./
  --view Cxx
  */
#include "StdTypes.hxx"

void StdTypes_nat::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_nat(this); }
uint32 StdTypes_nat::read(instream s_)
{
     uint32 ret_;
     ret_ = read_uint32(s_);
     return ret_;
}
void StdTypes_nat::write(uint32 x_, outstream s_)
{ write_uint32(x_, s_); }
uint32 StdTypes_nat::read_tagged(instream s_)
{
     uint32 ret_;
     if(read_tag(s_) != 4)die();
     ret_ = StdTypes_nat::read(s_);
     return ret_;
}
void StdTypes_nat::write_tagged(uint32 x_, outstream s_)
{
     write_tag(4, s_);
     StdTypes_nat::write(x_, s_);
}

void StdTypes_bool::accept(StdTypes_Visitor* v)
{ switch(this->kind()) {
      case StdTypes_bool::StdTypes_TRUE_enum: v->StdTypes_visit_TRUE(this);
      break;
      case StdTypes_bool::StdTypes_FALSE_enum: v->StdTypes_visit_FALSE(this);
      break;
      default: 
      die();
      
  } }
StdTypes_bool* StdTypes_bool::read(instream s_)
{
     StdTypes_bool* ret_;
     switch(read_tag(s_)) {
         case 1: ret_ = StdTypes_TRUE;
         break;
         case 2: ret_ = StdTypes_FALSE;
         break;
         default: 
         
         {
              ret_ = NULL;
              die();
         }
         
     }
     return ret_;
}
void StdTypes_bool::write(StdTypes_bool* x_, outstream s_)
{ 
{
     StdTypes_bool* t_;
     t_ = x_;
     switch(t_->kind()) {
         case StdTypes_bool::StdTypes_TRUE_enum: write_tag(1, s_);
         break;
         case StdTypes_bool::StdTypes_FALSE_enum: write_tag(2, s_);
         break;
         default: 
         die();
         
     }
} }
StdTypes_bool* StdTypes_bool::read_tagged(instream s_)
{
     StdTypes_bool* ret_;
     if(read_tag(s_) != 5)die();
     ret_ = StdTypes_bool::read(s_);
     return ret_;
}
void StdTypes_bool::write_tagged(StdTypes_bool* x_, outstream s_)
{
     write_tag(5, s_);
     StdTypes_bool::write(x_, s_);
}

void StdTypes_int8::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int8(this); }
int8 StdTypes_int8::read(instream s_)
{
     int8 ret_;
     ret_ = read_int8(s_);
     return ret_;
}
void StdTypes_int8::write(int8 x_, outstream s_) { write_int8(x_, s_); }
int8 StdTypes_int8::read_tagged(instream s_)
{
     int8 ret_;
     if(read_tag(s_) != 6)die();
     ret_ = StdTypes_int8::read(s_);
     return ret_;
}
void StdTypes_int8::write_tagged(int8 x_, outstream s_)
{
     write_tag(6, s_);
     StdTypes_int8::write(x_, s_);
}

void StdTypes_ieee_real::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_ieee_real(this); }
StdTypes_ieee_real* StdTypes_ieee_real::read(instream s_)
{
     StdTypes_ieee_real* ret_;
     
     {
          string string1;
          string1 = read_string(s_);
          ret_ = new StdTypes_ieee_real(string1);
     }
     return ret_;
}
void StdTypes_ieee_real::write(StdTypes_ieee_real* x_, outstream s_)
{ 
{
     StdTypes_ieee_real* t_;
     t_ = x_;
     write_string(t_->string1, s_);
} }
StdTypes_ieee_real* StdTypes_ieee_real::read_tagged(instream s_)
{
     StdTypes_ieee_real* ret_;
     if(read_tag(s_) != 14)die();
     ret_ = StdTypes_ieee_real::read(s_);
     return ret_;
}
void StdTypes_ieee_real::write_tagged(StdTypes_ieee_real* x_,
                                      outstream s_)
{
     write_tag(14, s_);
     StdTypes_ieee_real::write(x_, s_);
}

void StdTypes_int16::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int16(this); }
int16 StdTypes_int16::read(instream s_)
{
     int16 ret_;
     ret_ = read_int16(s_);
     return ret_;
}
void StdTypes_int16::write(int16 x_, outstream s_)
{ write_int16(x_, s_); }
int16 StdTypes_int16::read_tagged(instream s_)
{
     int16 ret_;
     if(read_tag(s_) != 7)die();
     ret_ = StdTypes_int16::read(s_);
     return ret_;
}
void StdTypes_int16::write_tagged(int16 x_, outstream s_)
{
     write_tag(7, s_);
     StdTypes_int16::write(x_, s_);
}

void StdTypes_int32::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int32(this); }
int32 StdTypes_int32::read(instream s_)
{
     int32 ret_;
     ret_ = read_int32(s_);
     return ret_;
}
void StdTypes_int32::write(int32 x_, outstream s_)
{ write_int32(x_, s_); }
int32 StdTypes_int32::read_tagged(instream s_)
{
     int32 ret_;
     if(read_tag(s_) != 8)die();
     ret_ = StdTypes_int32::read(s_);
     return ret_;
}
void StdTypes_int32::write_tagged(int32 x_, outstream s_)
{
     write_tag(8, s_);
     StdTypes_int32::write(x_, s_);
}

void StdTypes_int64::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int64(this); }
StdTypes_int64* StdTypes_int64::read(instream s_)
{
     StdTypes_int64* ret_;
     
     {
          int int1;
          int1 = read_int(s_);
          ret_ = new StdTypes_int64(int1);
     }
     return ret_;
}
void StdTypes_int64::write(StdTypes_int64* x_, outstream s_)
{ 
{
     StdTypes_int64* t_;
     t_ = x_;
     write_int(t_->int1, s_);
} }
StdTypes_int64* StdTypes_int64::read_tagged(instream s_)
{
     StdTypes_int64* ret_;
     if(read_tag(s_) != 9)die();
     ret_ = StdTypes_int64::read(s_);
     return ret_;
}
void StdTypes_int64::write_tagged(StdTypes_int64* x_, outstream s_)
{
     write_tag(9, s_);
     StdTypes_int64::write(x_, s_);
}

void StdTypes_uint8::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint8(this); }
uint8 StdTypes_uint8::read(instream s_)
{
     uint8 ret_;
     ret_ = read_uint8(s_);
     return ret_;
}
void StdTypes_uint8::write(uint8 x_, outstream s_)
{ write_uint8(x_, s_); }
uint8 StdTypes_uint8::read_tagged(instream s_)
{
     uint8 ret_;
     if(read_tag(s_) != 10)die();
     ret_ = StdTypes_uint8::read(s_);
     return ret_;
}
void StdTypes_uint8::write_tagged(uint8 x_, outstream s_)
{
     write_tag(10, s_);
     StdTypes_uint8::write(x_, s_);
}

void StdTypes_uint16::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint16(this); }
uint16 StdTypes_uint16::read(instream s_)
{
     uint16 ret_;
     ret_ = read_uint16(s_);
     return ret_;
}
void StdTypes_uint16::write(uint16 x_, outstream s_)
{ write_uint16(x_, s_); }
uint16 StdTypes_uint16::read_tagged(instream s_)
{
     uint16 ret_;
     if(read_tag(s_) != 11)die();
     ret_ = StdTypes_uint16::read(s_);
     return ret_;
}
void StdTypes_uint16::write_tagged(uint16 x_, outstream s_)
{
     write_tag(11, s_);
     StdTypes_uint16::write(x_, s_);
}
void StdTypes_uint32::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint32(this); }
uint32 StdTypes_uint32::read(instream s_)
{
     uint32 ret_;
     ret_ = read_uint32(s_);
     return ret_;
}
void StdTypes_uint32::write(uint32 x_, outstream s_)
{ write_uint32(x_, s_); }
uint32 StdTypes_uint32::read_tagged(instream s_)
{
     uint32 ret_;
     if(read_tag(s_) != 12)die();
     ret_ = StdTypes_uint32::read(s_);
     return ret_;
}
void StdTypes_uint32::write_tagged(uint32 x_, outstream s_)
{
     write_tag(12, s_);
     StdTypes_uint32::write(x_, s_);
}

void StdTypes_uint64::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint64(this); }
StdTypes_uint64* StdTypes_uint64::read(instream s_)
{
     StdTypes_uint64* ret_;
     
     {
          int int1;
          int1 = read_int(s_);
          ret_ = new StdTypes_uint64(int1);
     }
     return ret_;
}
void StdTypes_uint64::write(StdTypes_uint64* x_, outstream s_)
{ 
{
     StdTypes_uint64* t_;
     t_ = x_;
     write_int(t_->int1, s_);
} }
StdTypes_uint64* StdTypes_uint64::read_tagged(instream s_)
{
     StdTypes_uint64* ret_;
     if(read_tag(s_) != 13)die();
     ret_ = StdTypes_uint64::read(s_);
     return ret_;
}
void StdTypes_uint64::write_tagged(StdTypes_uint64* x_, outstream s_)
{
     write_tag(13, s_);
     StdTypes_uint64::write(x_, s_);
}
void StdTypes_nat_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_nat_list(this); }
StdTypes_nat_list* StdTypes_nat_list::read(instream s_)
{
     StdTypes_nat_list* ret_;
     
     {
          int t_1;
          StdTypes_nat_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_nat_list(StdTypes_nat::read(s_), NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_nat_list(StdTypes_nat::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_nat_list::write(StdTypes_nat_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_nat_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_nat::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_bool_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_bool_list(this); }
StdTypes_bool_list* StdTypes_bool_list::read(instream s_)
{
     StdTypes_bool_list* ret_;
     
     {
          int t_1;
          StdTypes_bool_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_bool_list(StdTypes_bool::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_bool_list(StdTypes_bool::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_bool_list::write(StdTypes_bool_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_bool_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_bool::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_int8_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int8_list(this); }
StdTypes_int8_list* StdTypes_int8_list::read(instream s_)
{
     StdTypes_int8_list* ret_;
     
     {
          int t_1;
          StdTypes_int8_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_int8_list(StdTypes_int8::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_int8_list(StdTypes_int8::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_int8_list::write(StdTypes_int8_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_int8_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_int8::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_ieee_real_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_ieee_real_list(this); }
StdTypes_ieee_real_list* StdTypes_ieee_real_list::read(instream s_)
{
     StdTypes_ieee_real_list* ret_;
     
     {
          int t_1;
          StdTypes_ieee_real_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_ieee_real_list(StdTypes_ieee_real::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_ieee_real_list(StdTypes_ieee_real::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_ieee_real_list::write(StdTypes_ieee_real_list* x_,
                                    outstream s_)
{ 
{
     int t_1;
     StdTypes_ieee_real_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_ieee_real::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_int16_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int16_list(this); }
StdTypes_int16_list* StdTypes_int16_list::read(instream s_)
{
     StdTypes_int16_list* ret_;
     
     {
          int t_1;
          StdTypes_int16_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_int16_list(StdTypes_int16::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_int16_list(StdTypes_int16::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_int16_list::write(StdTypes_int16_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_int16_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_int16::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_int32_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int32_list(this); }
StdTypes_int32_list* StdTypes_int32_list::read(instream s_)
{
     StdTypes_int32_list* ret_;
     
     {
          int t_1;
          StdTypes_int32_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_int32_list(StdTypes_int32::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_int32_list(StdTypes_int32::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_int32_list::write(StdTypes_int32_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_int32_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_int32::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_int64_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_int64_list(this); }
StdTypes_int64_list* StdTypes_int64_list::read(instream s_)
{
     StdTypes_int64_list* ret_;
     
     {
          int t_1;
          StdTypes_int64_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_int64_list(StdTypes_int64::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_int64_list(StdTypes_int64::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_int64_list::write(StdTypes_int64_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_int64_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_int64::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_uint8_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint8_list(this); }
StdTypes_uint8_list* StdTypes_uint8_list::read(instream s_)
{
     StdTypes_uint8_list* ret_;
     
     {
          int t_1;
          StdTypes_uint8_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_uint8_list(StdTypes_uint8::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_uint8_list(StdTypes_uint8::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_uint8_list::write(StdTypes_uint8_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_uint8_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_uint8::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_uint16_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint16_list(this); }
StdTypes_uint16_list* StdTypes_uint16_list::read(instream s_)
{
     StdTypes_uint16_list* ret_;
     
     {
          int t_1;
          StdTypes_uint16_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_uint16_list(StdTypes_uint16::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_uint16_list(StdTypes_uint16::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_uint16_list::write(StdTypes_uint16_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_uint16_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_uint16::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_uint32_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint32_list(this); }
StdTypes_uint32_list* StdTypes_uint32_list::read(instream s_)
{
     StdTypes_uint32_list* ret_;
     
     {
          int t_1;
          StdTypes_uint32_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_uint32_list(StdTypes_uint32::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_uint32_list(StdTypes_uint32::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_uint32_list::write(StdTypes_uint32_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_uint32_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_uint32::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_uint64_list::accept(StdTypes_Visitor* v)
{ v->StdTypes_visit_uint64_list(this); }
StdTypes_uint64_list* StdTypes_uint64_list::read(instream s_)
{
     StdTypes_uint64_list* ret_;
     
     {
          int t_1;
          StdTypes_uint64_list* t_2;
          t_1 = read_tag(s_);
          if(t_1 != 0)
              ret_ = new StdTypes_uint64_list(StdTypes_uint64::read(s_),
                      NULL);
          else
              return NULL;
          t_1 = t_1 - 1;
          t_2 = ret_;
          while(t_1 != 0)
          {
               t_2->tail = new StdTypes_uint64_list(StdTypes_uint64::read(s_),
                            NULL);
               t_2 = t_2->tail;
               t_1 = t_1 - 1;
          }
     }
     return ret_;
}
void StdTypes_uint64_list::write(StdTypes_uint64_list* x_, outstream s_)
{ 
{
     int t_1;
     StdTypes_uint64_list* t_2;
     t_1 = 0;
     t_2 = x_;
     while(t_2 != NULL)
     {
          t_2 = t_2->tail;
          t_1 = t_1 + 1;
     }
     write_tag(t_1, s_);
     t_2 = x_;
     while(t_1 != 0)
     {
          StdTypes_uint64::write(t_2->head, s_);
          t_2 = t_2->tail;
          t_1 = t_1 - 1;
     }
} }
void StdTypes_Visitor::StdTypes_visit_nat(StdTypes_nat* x);
void StdTypes_Visitor::StdTypes_visit_TRUE(StdTypes_bool* x);
void StdTypes_Visitor::StdTypes_visit_FALSE(StdTypes_bool* x);
void StdTypes_Visitor::StdTypes_visit_int8(StdTypes_int8* x);
void StdTypes_Visitor::StdTypes_visit_ieee_real(StdTypes_ieee_real* x);
void StdTypes_Visitor::StdTypes_visit_int16(StdTypes_int16* x);
void StdTypes_Visitor::StdTypes_visit_int32(StdTypes_int32* x);
void StdTypes_Visitor::StdTypes_visit_int64(StdTypes_int64* x);
void StdTypes_Visitor::StdTypes_visit_uint8(StdTypes_uint8* x);
void StdTypes_Visitor::StdTypes_visit_uint16(StdTypes_uint16* x);
void StdTypes_Visitor::StdTypes_visit_uint32(StdTypes_uint32* x);
void StdTypes_Visitor::StdTypes_visit_uint64(StdTypes_uint64* x);
void StdTypes_Visitor::StdTypes_visit_nat_list(StdTypes_nat_list* x);
void StdTypes_Visitor::StdTypes_visit_bool_list(StdTypes_bool_list* x);
void StdTypes_Visitor::StdTypes_visit_int8_list(StdTypes_int8_list* x);
void StdTypes_Visitor::StdTypes_visit_ieee_real_list(StdTypes_ieee_real_list* x);
void StdTypes_Visitor::StdTypes_visit_int16_list(StdTypes_int16_list* x);
void StdTypes_Visitor::StdTypes_visit_int32_list(StdTypes_int32_list* x);
void StdTypes_Visitor::StdTypes_visit_int64_list(StdTypes_int64_list* x);
void StdTypes_Visitor::StdTypes_visit_uint8_list(StdTypes_uint8_list* x);
void StdTypes_Visitor::StdTypes_visit_uint16_list(StdTypes_uint16_list* x);
void StdTypes_Visitor::StdTypes_visit_uint32_list(StdTypes_uint32_list* x);
void StdTypes_Visitor::StdTypes_visit_uint64_list(StdTypes_uint64_list* x);
StdTypes_bool* StdTypes_TRUE = new StdTypes_bool(StdTypes_bool::StdTypes_TRUE_enum);
StdTypes_bool* StdTypes_FALSE = new StdTypes_bool(StdTypes_bool::StdTypes_FALSE_enum);


