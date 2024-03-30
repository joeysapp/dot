# defaults domains | tr ',' '\n';
for i in `defaults domains | tr ',' '\n'`;
do
      echo "- $i";
      defaults read $i;
      echo "- $i\n\n";
done
defaults domains | tr ',' '\n';

# https://apple.stackexchange.com/questions/195244/concise-compact-list-of-all-defaults-currently-configured-and-their-values
