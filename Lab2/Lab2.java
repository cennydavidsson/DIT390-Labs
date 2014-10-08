import TSim.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab1 {
    
    final private int topSpeed = 17;

    public static void main(String[] args)  throws InterruptedException{
        new Lab1(args);
    }
    
    public Lab1(String[] args) throws InterruptedException{

        int trainSpeed1 = (int) (args.length >= 1 ? Integer.parseInt(args[0]) : Math.ceil(Math.random()*topSpeed));
        int trainSpeed2 = (int) (args.length >= 2 ? Integer.parseInt(args[1]) : Math.ceil(Math.random()*topSpeed));
        int simSpeed    = args.length        >= 3 ? Integer.parseInt(args[2]) : 100;
        
        List<Monitor> monList = new ArrayList<>();
        for (int i = 0; i < 6; i++) {
            monList.add(new Monitor());
        }
        
        TrainThread t1 = new TrainThread(1, trainSpeed1, (ArrayList<Monitor>) monList, simSpeed);
        new Thread(t1).start();
        
        TrainThread t2 = new TrainThread(2, trainSpeed2, (ArrayList<Monitor>) monList, simSpeed);
        new Thread(t2).start();   
    }
}

class TrainThread implements Runnable {
    
    public enum Direction { 
        up, down 
    }
    
    int id, speed, speedparameter;
    Monitor monTrack1, monTrack3, monCrossing, monMiddleHigh, monSensor6, monSensor9;
    TSimInterface tsi = TSimInterface.getInstance();
    Boolean defaultTrack = true;
    
    TrainThread(int trainID, int startspeed, ArrayList<Monitor> monList, int speedparameter) throws InterruptedException {
        id                  = trainID;
        speed               = startspeed;
        this.speedparameter = speedparameter;

        monTrack1           = monList.get(0);
        monTrack3           = monList.get(1);
        monCrossing         = monList.get(2);
        monMiddleHigh       = monList.get(3);
        monSensor6          = monList.get(4);
        monSensor9          = monList.get(5);
        
        (id == 1 ? monTrack1 : monTrack3).enter();
    }
    
    @Override
    public void run() {
        try {
            runTrain();
        } catch (InterruptedException ex) {
            Logger.getLogger(TrainThread.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public void runTrain() throws InterruptedException {
        
        try {
            tsi.setDebug(true);
            tsi.setSpeed(id, speed);
                      
            while(true) {
                SensorEvent sensorEvent = tsi.getSensor(id);
                
                if (id == sensorEvent.getTrainId()) {
                    int activeSensor = getActiveSensorFor(sensorEvent);
                    switch (activeSensor) {
                        case 1:
                            if (goingUp(id)) {
                                changeDirection(id);
                            }
                            break;
                            
                        case 2:
                            if (goingDown(id)) {
                                if (!monCrossing.tryEnter())  {
                                    getMonCrossing(id, speed);
                                }
                            }
                            if (goingUp(id)) {
                                monCrossing.leave();
                            }
                            break;
                            
                        case 3:
                            if (goingUp(id)) {
                                monCrossing.leave();
                                changeDirection(id);
                            }
                            
                            if (goingDown(id)) {
                                if (!monCrossing.tryEnter()) {
                                    getMonCrossing(id, speed);
                                }
                            }
                            break;
                            
                        case 4: case 5:
                            if (goingDown(id)) {
                                monCrossing.leave();
                                enterMonWithWait(monSensor6);
                                setSwitch(1, activeSensor == 4 ? Direction.up : Direction.down);
                            }
                            if (goingUp(id)) {
                                monSensor6.leave();
                                if (!monCrossing.tryEnter()) {
                                    getMonCrossing(id, speed);
                                }
                            }
                            break;
                            
                        case 6:
                            if (goingDown(id)) {
                               tryleaveAndEnterMonAtSwitch(monTrack1, monMiddleHigh, 3);                                
                            }
                            if (goingUp(id)) {
                               tryleaveAndEnterMonAtSwitch(monMiddleHigh, monTrack1, 1);                                 
                            }
                            break;
                            
                        case 7: case 10:
                            if (goingDown(id)) {
                                enterMonWithWait(monSensor9);
                                setSwitch(2, activeSensor == 7 ? Direction.up : Direction.down);
                            }
                            if (goingUp(id)) {
                                monSensor9.leave();
                            }
                            break;
                            
                        case 8: case 11:
                            if (goingDown(id)) {
                                monSensor6.leave();
                            }
                            if (goingUp(id)) {
                                enterMonWithWait(monSensor6);
                                setSwitch(3, activeSensor == 8 ? Direction.up : Direction.down);
                            }
                            break;
                            
                        case 9:
                            if (goingUp(id)) {
                                tryleaveAndEnterMonAtSwitch(monTrack3, monMiddleHigh, 2);
                            }
                            if (goingDown(id)) {
                                tryleaveAndEnterMonAtSwitch(monMiddleHigh, monTrack3, 4);
                            }
                            break;
                            
                        case 12: case 14:
                            if (goingDown(id))  {
                                monSensor9.leave();
                            }
                            if (goingUp(id)) {
                                enterMonWithWait(monSensor9);
                                setSwitch(4, activeSensor == 12 ? Direction.up : Direction.down);
                            }
                            break;
                            
                        case 13: case 15:
                            if (goingDown(id)) {
                                changeDirection(id);
                            }
                            break;
     
                        default: break;
                    }
                }
            }
        }
        catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }
    }
    
    private int getActiveSensorFor(SensorEvent sensorEvent) {
        
        int x = sensorEvent.getXpos();
        int y = sensorEvent.getYpos();
        
        if ( sensorEvent.getStatus() == 1 ) {
            if (x == 14 && y ==  3 ) { return  1; }
            if (x ==  6 && y ==  5 ) { return  2; }
            if (x == 14 && y ==  5 ) { return  3; }
            if (x == 14 && y ==  7 ) { return  4; }
            if (x == 14 && y ==  8 ) { return  5; }
            if (x == 19 && y ==  8 ) { return  6; }
            if (x ==  6 && y ==  9 ) { return  7; }
            if (x == 13 && y ==  9 ) { return  8; }
            if (x ==  1 && y == 10 ) { return  9; }
            if (x ==  6 && y == 10 ) { return 10; }
            if (x == 13 && y == 10 ) { return 11; }
            if (x ==  5 && y == 11 ) { return 12; }
            if (x == 14 && y == 11 ) { return 13; }
            if (x ==  5 && y == 13 ) { return 14; }
            if (x == 14 && y == 13 ) { return 15; }
        }
        return 0;
    }
    
    private void setSwitch(int trackSwitch, Direction dir) throws CommandException {
        switch (trackSwitch) {
            case 1:
                tsi.setSwitch(17, 7, dir == Direction.up ? 2 : 1);
                break;
        
            case 2:
                tsi.setSwitch(4, 9, dir == Direction.up ? 1 : 2);
                break;
                
            case 3:
                tsi.setSwitch(15, 9, dir == Direction.up ? 2 : 1);
                break;
                
            case 4:
                tsi.setSwitch(3, 11, dir == Direction.up ? 1 : 2);
                break;
            
            default: break;
        }
    }
    
    private void changeDirection(int i) throws CommandException, InterruptedException {
        tsi.setSpeed(i, 0);
        speed *= -1;
        Thread.sleep(1000+2*speedparameter*Math.abs(speed));
        tsi.setSpeed(i, speed);
    }
    
    private Boolean goingUp(int trainID) {
        return (trainID == 1 && speed < 0) || (trainID == 2 && speed > 0);
    }
    
    private Boolean goingDown(int trainID) {
        return (trainID == 1 && speed > 0) || (trainID == 2 && speed < 0);
    }
    
    private void getMonCrossing(int id, int speed) throws CommandException, InterruptedException {
        Boolean canAcquire = false;
        tsi.setSpeed(id,0);
        while (!canAcquire) {
            canAcquire = monCrossing.tryEnter();
        }        
        
        tsi.setSpeed(id,speed);
    }

    private void enterMonWithWait(Monitor sem) throws CommandException, InterruptedException {
        Boolean canAcquire = false;
        if (!sem.tryEnter()) {
            tsi.setSpeed(id, 0);
            while (!canAcquire) {
                canAcquire = sem.tryEnter();
            }
            tsi.setSpeed(id, speed);
        }
    }

    private void tryleaveAndEnterMonAtSwitch(Monitor leaveMon, Monitor enterMon, int railsSwitch) throws CommandException, InterruptedException {
        if (defaultTrack) {
            leaveMon.leave();
            defaultTrack = false;
        }
        if (enterMon.tryEnter()) {
            setSwitch(railsSwitch, Direction.up);
            defaultTrack = true;            
        } else {
            setSwitch(railsSwitch, Direction.down);
        }
    }
}

class Monitor {
        final Lock lock            = new ReentrantLock();
        final Condition cond       = lock.newCondition(); 
        private Boolean isOnTrack  = false;

        public void enter() throws InterruptedException {
            lock.lock();
            try {
                while (isOnTrack) {
                    cond.await();
                }
                isOnTrack = true;
            } finally {
                lock.unlock();
            }
        }

        public Boolean tryEnter() throws InterruptedException {
            lock.lock();
            try {
                if (isOnTrack) {
                    return false;
                } else {
                    isOnTrack = true;
                    return isOnTrack;
                }
            } finally {
                lock.unlock();
            }
        }

        public void leave() throws InterruptedException {
            lock.lock();
            try {
                isOnTrack = false;
                cond.signal();
            } finally {
                lock.unlock();
            }
        }
    }
