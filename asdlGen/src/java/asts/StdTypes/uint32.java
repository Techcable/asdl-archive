/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_import asdl_base
  --line_width 74
  --no_action false
  --output_directory ./
  --view Java
  */
package asts.StdTypes;
import asdl_base.*;
public final class uint32 {
    public int int1;
    
    public uint32(int int1)
    {
        this.int1 = int1;
    }
    public final static void write_option(uint32 x_,
                                          java.io.OutputStream s_)
     {
         if(x_ != null)
             {
                 g.write_tag(1, s_);
                 uint32.write(x_, s_);
             }
          else
             g.write_tag(0, s_);
     }
    public final static uint32 read_option(java.io.InputStream s_)
     {
         uint32 ret_;
         if(g.read_tag(s_) != 0) ret_ = uint32.read(s_); else ret_ = null;
         return ret_;
     }
    public final void accept(Visitor v)
     {
         v.visit_uint32(this);
     }
    public final static uint32 read(java.io.InputStream s_)
     {
         uint32 ret_;
         {
             int int1;
             int1 = g.read_int(s_);
             ret_ = new uint32(int1);
         }
         return ret_;
     }
    public final static void write(uint32 x_, java.io.OutputStream s_)
     {
         {
             uint32 t_;
             t_ = x_;
             g.write_int(t_.int1, s_);
         }
     }
    public final static uint32 read_tagged(java.io.InputStream s_)
     {
         uint32 ret_;
         if(g.read_tag(s_) != 12) g.die();
         ret_ = uint32.read(s_);
         return ret_;
     }
    public final static void write_tagged(uint32 x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(12, s_);
         uint32.write(x_, s_);
     }
}