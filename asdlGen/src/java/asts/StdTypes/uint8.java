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
public final class uint8 {
    public int int1;
    
    public uint8(int int1)
    {
        this.int1 = int1;
    }

    public final void accept(Visitor v)
     {
         v.visit_uint8(this);
     }
    public final static short read(java.io.InputStream s_)
     {
         short ret_;
         ret_ = g.read_java_short(s_);
         return ret_;
     }
    public final static void write(short x_, java.io.OutputStream s_)
     {
         g.write_java_short(x_, s_);
     }
    public final static short read_tagged(java.io.InputStream s_)
     {
         short ret_;
         if(g.read_tag(s_) != 10) g.die();
         ret_ = uint8.read(s_);
         return ret_;
     }
    public final static void write_tagged(short x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(10, s_);
         uint8.write(x_, s_);
     }
}
