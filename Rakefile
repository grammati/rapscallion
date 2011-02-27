
MVN = "#{File.expand_path('~')}/.m2/repository"

def windows?
    RUBY_PLATFORM =~ /win|mingw/
end

def path_sep
    windows? ? ';' : ':'
end

def classpath
    paths = ['src', 'test']
    paths += Dir['./lib/*.jar']
    paths += Dir['./lib/dev/*.jar']
    paths = paths.map{|f| File.expand_path f }
    if windows?
        paths = paths.map{|f| f.gsub(/\//, '\\') }  # java doesn't like forward slashed on windows!!
    end
    paths = paths.join(path_sep)
end

def java_cmd
    %Q{java -cp #{classpath} #{ENV['JVM_OPTS']} }
end

def java(args)
    cmd = "#{java_cmd} #{args}"
    puts "Executing: ", cmd
    exec cmd
end

task :deps do
    libs = ['org/clojure/clojure/1.2.0-master-SNAPSHOT/clojure-1.2.0-master-SNAPSHOT.jar', 
            'swank-clojure/swank-clojure/1.2.1/swank-clojure-1.2.1.jar']
    libs.each do |lib|
      puts `cp "#{MVN}/#{lib}" "#{Dir.pwd}/lib/"`
    end
end

task :env do
    puts classpath
end

task :repl do
    cmd = %Q{#{windows? ? '' : 'rlwrap '}#{java_cmd} clojure.main}
    puts "Executing: ", cmd
    exec cmd
end

task :test do
    test_cmd = %Q{-e "(use 'clojure.test '(rapscallion.test core samples)) (run-tests 'rapscallion.test.core 'rapscallion.test.samples)"}
    java "clojure.main #{test_cmd}"
end

task :gui do
    java "clojure.main ./tools/rapscallion/gui.clj"
end



