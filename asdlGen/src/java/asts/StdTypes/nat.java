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
public final class nat {
    public int int1;
    
    public nat(int int1)
    {
        this.int1 = int1;
    }
    public final static void write_option(nat x_, java.io.OutputStream s_)
     {
         if(x_ != null)
             {
                 g.write_tag(1, s_);
                 nat.write(x_, s_);
             }
          else
             g.write_tag(0, s_);
     }
    public final static nat read_option(java.io.InputStream s_)
     {
         nat ret_;
         if(g.read_tag(s_) != 0) ret_ = nat.read(s_); else ret_ = null;
         return ret_;
     }
    public final void accept(Visitor v)
     {
         v.visit_nat(this);
     }
    public final static nat read(java.io.InputStream s_)
     {
         nat ret_;
         {
             int int1;
             int1 = g.read_int(s_);
             ret_ = new nat(int1);
         }
         return ret_;
     }
    public final static void write(nat x_, java.io.OutputStream s_)
     {
         {
             nat t_;
             t_ = x_;
             g.write_int(t_.int1, s_);
         }
     }
    public final static nat read_tagged(java.io.InputStream s_)
     {
         nat ret_;
         if(g.read_tag(s_) != 4) g.die();
         ret_ = nat.read(s_);
         return ret_;
     }
    public final static void write_tagged(nat x_, java.io.OutputStream s_)
     {
         g.write_tag(4, s_);
         nat.write(x_, s_);
     }
}