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
public final class int8 {
    public int int1;
    
    public int8(int int1)
    {
        this.int1 = int1;
    }

    public final void accept(Visitor v)
     {
         v.visit_int8(this);
     }
    public final static byte read(java.io.InputStream s_)
     {
         byte ret_;
         ret_ = g.read_java_byte(s_);
         return ret_;
     }
    public final static void write(byte x_, java.io.OutputStream s_)
     {
         g.write_java_byte(x_, s_);
     }
    public final static byte read_tagged(java.io.InputStream s_)
     {
         byte ret_;
         if(g.read_tag(s_) != 6) g.die();
         ret_ = int8.read(s_);
         return ret_;
     }
    public final static void write_tagged(byte x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(6, s_);
         int8.write(x_, s_);
     }
}
