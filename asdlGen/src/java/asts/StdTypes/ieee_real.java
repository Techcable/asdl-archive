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
public final class ieee_real {
    public String string1;
    
    public ieee_real(String string1)
    {
        this.string1 = string1;
    }

    public final void accept(Visitor v)
     {
         v.visit_ieee_real(this);
     }
    public final static double read(java.io.InputStream s_)
     {
         double ret_;
         ret_ = g.read_java_double(s_);
         return ret_;
     }
    public final static void write(double x_, java.io.OutputStream s_)
     {
         g.write_java_double(x_, s_);
     }
    public final static double read_tagged(java.io.InputStream s_)
     {
         double ret_;
         if(g.read_tag(s_) != 14) g.die();
         ret_ = ieee_real.read(s_);
         return ret_;
     }
    public final static void write_tagged(double x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(14, s_);
         ieee_real.write(x_, s_);
     }
}
