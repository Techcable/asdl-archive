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
public final class nat_list {
    public nat head;
    public nat_list tail;
    
    public nat_list(nat head, nat_list tail)
    {
        this.head = head;
        this.tail = tail;
    }
    public final void accept(Visitor v)
     {
         v.visit_nat_list(this);
     }
    public final static nat_list read(java.io.InputStream s_)
     {
         nat_list ret_;
         {
             int t_1;
             nat_list t_2;
             t_1 = g.read_tag(s_);
             if(t_1 != 0)
                 ret_ = new nat_list(nat.read(s_), null);
              else
                 return null;
             t_1 = t_1 - 1;
             t_2 = ret_;
             while(t_1 != 0)
             {
                 t_2.tail = new nat_list(nat.read(s_), null);
                 t_2 = t_2.tail;
                 t_1 = t_1 - 1;
             }
         }
         return ret_;
     }
    public final static void write(nat_list x_, java.io.OutputStream s_)
     {
         {
             int t_1;
             nat_list t_2;
             t_1 = 0;
             t_2 = x_;
             while(t_2 != null)
             {
                 t_2 = t_2.tail;
                 t_1 = t_1 + 1;
             }
             g.write_tag(t_1, s_);
             t_2 = x_;
             while(t_1 != 0)
             {
                 nat.write(t_2.head, s_);
                 t_2 = t_2.tail;
                 t_1 = t_1 - 1;
             }
         }
     }
}