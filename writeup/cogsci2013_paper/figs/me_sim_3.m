% M. Lewis 12.26.12
clear all
addpath('helper','score','..')

% Input
world.words = [1:2];
world.features = 1; 
world.feature_levels = 2; % are features binary or trinary? (trinary for antenna)
world.theories = {'1-1','1-many','many-1','inconsistent'};
world.size_principle = 0;
world.intention = 1;
world.alpha = .1;
world.epsilon = .1;

createWorld;

ns = [.9 .7 .5];

for n = 1:3
  world.alpha = ns(n);
  world.epsilon = ns(n);

  createWorld;
  
  for d = 1:3
    clear data
    data(1).word = 1;
    data(1).objects = [1];

    for i = 1:d-1
      data(end+1).word = 1;
      data(end).objects = [1];
    end

    data(end+1).word = 2;
    data(end).objects = [1; 2];

    for t=1:length(world.theories) 
      theories.semantic = world.theories{t};

      % Output: 
      [lexicons_post(t) lexicons world] = learner (world, data, theories,...
          'all');
%       disp([world.theories{t} ', lexicons: ' num2str(sum(lexicons.include))]);


      corr = getLexPredics(world, lexicons_post(t).normalized, lexicons, 1, 2);
      incorr = getLexPredics(world, lexicons_post(t).normalized, lexicons, 0, 2);
      p_D(d,n,t) = corr / (corr + incorr);
    
      
      full_post = sum([lexicons_post(:).unnormalized],2) ./ repmat(sum(sum([lexicons_post(:).unnormalized],2)),size(lexicons.lexs,3),1);
      
      corr = getLexPredics(world, full_post, lexicons, 1, 2);
      incorr = getLexPredics(world, full_post, lexicons, 0, 2);
      p_D_full(d,n,t) = corr / (corr + incorr);

    
    end

  end
end



%% MAKE PLOTS WITH EXPERIMENTAL DATA
me_label_means=csvread('/Documents/GRADUATE_SCHOOL/Projects/redac_proj/redac2/sims/data/me_label_means.csv',1,1);
me_label_sems=csvread('/Documents/GRADUATE_SCHOOL/Projects/redac_proj/redac2/sims/data/me_label_sems.csv',1,1);

% PLOT
fs = 15; %font size
fs2 = 20;
ms = 20
clf;

%Model predictions
subplot(1,2,1)
plot(p_D(:,:,1),'.-', 'MarkerSize', ms)
axis([.5 3.5 0 1])
line([0 5],[.5 .5],'Color',[0 0 0],'LineStyle','--')
set(gca,'XTick',1:3)
xlabel('Number of Labels', 'FontSize', fs)
ylabel('Proportion Correct', 'FontSize', fs)
title('Model Predictions', 'FontSize', fs2)
legend({'Low noise', 'Medium Noise', 'High Noise'}, 'Location', 'Southeast')

% Experimental data
subplot(1,2,2)
errorbar([1 2 3], me_label_means(1,:)/100, me_label_sems(1,:), -me_label_sems(1,:), 'b.-', 'MarkerSize', ms)
hold on;
errorbar([1 2 3], me_label_means(2,:)/100, me_label_sems(2,:), -me_label_sems(2,:), 'g.-', 'MarkerSize', ms)
hold on;
errorbar([1 2 3], me_label_means(3,:)/100, me_label_sems(3,:), -me_label_sems(3,:), 'r.-', 'MarkerSize', ms)
set(gca,'XTick',1:3)
axis([.5 3.5 0 1])
line([0 5],[.5 .5],'Color',[0 0 0],'LineStyle','--')
legend({'2-3 years', '3-4 years', '4-5 years'}, 'Location', 'Southeast')
title('Experimental Data', 'FontSize', fs2)
xlabel('Number of Labels', 'FontSize', fs)
ylabel('Proportion Correct', 'FontSize', fs)

