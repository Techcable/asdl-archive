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
public final class uint16 {
    public int int1;
    
    public uint16(int int1)
    {
        this.int1 = int1;
    }

    public final void accept(Visitor v)
     {
         v.visit_uint16(this);
     }
    public final static int read(java.io.InputStream s_)
     {
         int ret_;
         ret_ = g.read_java_int(s_);
         return ret_;
     }
    public final static void write(int x_, java.io.OutputStream s_)
     {
         g.write_java_int(x_, s_);
     }
    public final static int read_tagged(java.io.InputStream s_)
     {
         int ret_;
         if(g.read_tag(s_) != 11) g.die();
         ret_ = uint16.read(s_);
         return ret_;
     }
    public final static void write_tagged(int x_, java.io.OutputStream s_)
     {
         g.write_tag(11, s_);
         uint16.write(x_, s_);
     }
}
