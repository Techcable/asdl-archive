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
public final class int32 {
    public int int1;
    
    public int32(int int1)
    {
        this.int1 = int1;
    }

    public final void accept(Visitor v)
     {
         v.visit_int32(this);
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
         if(g.read_tag(s_) != 8) g.die();
         ret_ = int32.read(s_);
         return ret_;
     }
    public final static void write_tagged(int x_, java.io.OutputStream s_)
     {
         g.write_tag(8, s_);
         int32.write(x_, s_);
     }
}
