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
public final class int64 {
    public int int1;
    
    public int64(int int1)
    {
        this.int1 = int1;
    }

    public final void accept(Visitor v)
     {
         v.visit_int64(this);
     }
    public final static long read(java.io.InputStream s_)
     {
         long ret_;
         ret_ = g.read_java_long(s_);
         return ret_;
     }
    public final static void write(long x_, java.io.OutputStream s_)
     {
         g.write_java_long(x_, s_);
     }
    public final static long read_tagged(java.io.InputStream s_)
     {
         long ret_;
         if(g.read_tag(s_) != 9) g.die();
         ret_ = int64.read(s_);
         return ret_;
     }
    public final static void write_tagged(long x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(9, s_);
         int64.write(x_, s_);
     }
}
